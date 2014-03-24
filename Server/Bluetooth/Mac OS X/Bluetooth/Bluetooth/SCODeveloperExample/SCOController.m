/*
	IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in
	consideration of your agreement to the following terms, and your use, installation, 
	modification or redistribution of this Apple software constitutes acceptance of these 
	terms.  If you do not agree with these terms, please do not use, install, modify or 
	redistribute this Apple software.
	
	In consideration of your agreement to abide by the following terms, and subject to these 
	terms, Apple grants you a personal, non-exclusive license, under Apple’s copyrights in 
	this original Apple software (the "Apple Software"), to use, reproduce, modify and 
	redistribute the Apple Software, with or without modifications, in source and/or binary 
	forms; provided that if you redistribute the Apple Software in its entirety and without 
	modifications, you must retain this notice and the following text and disclaimers in all 
	such redistributions of the Apple Software.  Neither the name, trademarks, service marks 
	or logos of Apple Computer, Inc. may be used to endorse or promote products derived from 
	the Apple Software without specific prior written permission from Apple. Except as expressly
	stated in this notice, no other rights or licenses, express or implied, are granted by Apple
	herein, including but not limited to any patent rights that may be infringed by your 
	derivative works or by other works in which the Apple Software may be incorporated.
	
	The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO WARRANTIES, 
	EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED WARRANTIES OF NON-INFRINGEMENT, 
	MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS 
	USE AND OPERATION ALONE OR IN COMBINATION WITH YOUR PRODUCTS.
	
	IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR CONSEQUENTIAL 
	DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS 
			 OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) ARISING IN ANY WAY OUT OF THE USE, 
	REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE APPLE SOFTWARE, HOWEVER CAUSED AND 
	WHETHER UNDER THEORY OF CONTRACT, TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR 
	OTHERWISE, EVEN IF APPLE HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#import <IOBluetooth/IOBluetooth.h>
#import <IOBluetoothUI/IOBluetoothUI.h>

#import <IOKit/audio/IOAudioTypes.h>

#import "MyHandsFreeGateway.h"

#import "SCOController.h"

extern BluetoothDeviceAddress	gBDADDR;
extern IOBluetoothObjectID		gObjectID;

#define Log( FMT, ARGS...)		NSLog( @ FMT, ## ARGS)

#define kDefaultFinishConnectionTimeout		5.0
#define kDefaultServiceLevelTimeout			2.0

enum
{
	kBTAudioError	= 1
};


@implementation SCOController

//===================================================================================================
//
//===================================================================================================
- (id) init
{	
	self = [super init];
	
	mIsDoneLaunching = NO;
	
	return self;
}
	

//===================================================================================================
// applicationDidFinishLaunching
//===================================================================================================
- (void) applicationDidFinishLaunching:(NSNotification *)notification
{		
	if( IOBluetoothValidateHardware( nil ) != kIOReturnSuccess )
	{
		[NSApp terminate:self];
	}
}


//===================================================================================================
//   
//===================================================================================================
- (BOOL) wasLaunchedFromIncomingDevice
{
	return (gObjectID != 0);
}


#pragma mark -
//===================================================================================================
//
//===================================================================================================
- (IOReturn) setFinishConnectionTimer
{	
	[self resetFinishConnectionTimer];
	
	if( mIsDoneLaunching )
	{  
		Log("[refreshFinishConnectionTimer] already done launching!\n");
		return kIOReturnSuccess; 
	}
	
	Log("[refreshFinishConnectionTimer] refreshing timer...\n");
	
	mFinishConnectionTimer = [NSTimer scheduledTimerWithTimeInterval: kDefaultFinishConnectionTimeout
															  target: self
															selector: @selector(handleFinishConnectionTimeout:)
															userInfo: nil
															 repeats: NO];
	if( mFinishConnectionTimer )
	{
		[mFinishConnectionTimer retain];
	}
	
	return kIOReturnSuccess;
}

//===================================================================================================
- (IOReturn) resetFinishConnectionTimer
{
	if( mFinishConnectionTimer )
	{
		[mFinishConnectionTimer invalidate];
		[mFinishConnectionTimer release];
		mFinishConnectionTimer = nil;
	}
	
	return kIOReturnSuccess;
}

//===================================================================================================
- (void) handleFinishConnectionTimeout:(id)sender
{
	[self resetFinishConnectionTimer];
	
	[self shutdown];
}




#pragma mark -
//===================================================================================================
//  Connect SCO
//===================================================================================================
- (void) prepareSCOAudioServices
{
	IOBluetoothDevice * device;
	IOBluetoothRFCOMMChannel * channel;
	BluetoothRFCOMMChannelID channelID;
	IOReturn err;
	err = kIOReturnSuccess;
	
	if( [self wasLaunchedFromIncomingDevice] )  
	{
		//
		// Incoming device connection, we were launched from blued
		//
		channel		= [IOBluetoothRFCOMMChannel withObjectID: gObjectID];
		device		= [channel getDevice];
		channelID	= [channel getChannelID];
				
		Log("[connectSCO] incoming rfcomm channel id: %d\n", channelID);
		
		//
		//  Try to instantiate a handler object for the incoming connection.  Each init method
		// will look at the incoming RFCOMM channel and see what service the device is trying 
		// to attach to and will return nil if the device is not attaching to the proper Gateway 
		// record
		//
		Log("[connectSCO] Trying HS profile...\n");
		mHeadsetDevice = [[IOBluetoothHeadsetDevice alloc] initWithIncomingDevice: device
														  incomingRFCOMMChannelID: channelID
																		 delegate: self];
		if( !mHeadsetDevice )
		{
			Log("[connectSCO] Trying HFGW profile...\n");
			mHandsFreeGateway = [[MyHandsFreeGateway alloc] initWithIncomingDevice: device
														   incomingRFCOMMChannelID: channelID
																 supportedFeatures: kBluetoothHandsFreeFeatureNone
																		  delegate: self];
		}
		
		// it must have the service AND be trying to connect to the service
		//
		if( !mHandsFreeGateway && !mHeadsetDevice )
		{
			Log("[connectSCO] ERR: Strange, incoming device is not attaching to either HFAG or HSAG. Im outta here!\n");
			err = kIOReturnNoDevice;
			goto exit;
		}
				
		//  Setup a timer to make sure the device finishes its connection with us
		if( mIsDoneLaunching == FALSE )
		{
			[self setFinishConnectionTimer];
		}
	}
	else 
	{
		// Pending audio, need to connect to device, we were launched from the driver
		//
		// Outgoing RFCOMM channel will be pulled from the SDP record in this device
		channel		= nil;
		device		= [IOBluetoothDevice withAddress: &gBDADDR];
		channelID	= 0;
		
		if( [IOBluetoothHeadsetDevice getRequiredSDPServiceRecordForDevice: device] != nil )
		{
			Log("[connectSCO] Using HS profile...\n");
			mHeadsetDevice = [[IOBluetoothHeadsetDevice alloc] initForConnectionToDevice: device
																				delegate: self];
		}
		else if( [MyHandsFreeGateway getRequiredSDPServiceRecordForDevice: device] != nil )
		{
			Log("[connectSCO] Using HFGW profile...\n");
			mHandsFreeGateway = [[MyHandsFreeGateway alloc] initForConnectionToDevice: device
																			 supportedFeatures: kBluetoothHandsFreeFeatureNone
																					  delegate: self];
		}
		else
		{
			Log("[connectSCO] ERR: incoming device can't play with us!\n");
			err = kIOReturnNoDevice;
			goto exit;
		}		
	}
	
	// make sure we have an object of some kind...
	if( ! mHandsFreeGateway && ! mHeadsetDevice )
	{
		Log("[connectSCO] ERR: SCO devices are nil, there was an error!\n");
		err = kIOReturnNoDevice;
		goto exit;
	}
	
	Log("[connectSCO] objID: %d  channel: %p  chanID: %d\n", gObjectID, channel, channelID );
	
	if( mIsDoneLaunching == FALSE )
	{
		[self setPlayOrDieTimer];
	}
	
exit:
	if( err != kIOReturnSuccess )
	{
		[self shutdownWithError: err];
	}

	return;
}




//===================================================================================================
//  shutdown
//===================================================================================================
- (void) shutdown
{
	[self shutdownWithError: kIOReturnSuccess];
}

//===================================================================================================
//  shutdownWithError
//===================================================================================================
- (void) shutdownWithError:(IOReturn)inError
{		
	Log("Disconnecting...\n");
	
	[self resetPlayOrDieTimer];	
	
	if( mHandsFreeGateway )
	{
		[mHandsFreeGateway closeDeviceConnection];
		[mHandsFreeGateway release];
		mHandsFreeGateway = nil;
	}
	
	if( mHeadsetDevice )
	{
		[mHeadsetDevice closeDeviceConnection];
		[mHeadsetDevice release];
		mHeadsetDevice = nil;
	}
	
	if( inError )
	{
		NSRunCriticalAlertPanel( @"Bluetooth audio failed",
								 @"There was an error connecting to your headset.  Make sure it is turned on and in range.  The audio portion of the program you were using may have to be restarted.", 
								 @"OK",
								 nil,
								 nil);
	}

	[NSApp terminate: self];

}


#pragma mark -
#pragma mark === timers ===
//===================================================================================================
//
//===================================================================================================
- (IOReturn) setPlayOrDieTimer
{
	[self resetPlayOrDieTimer];
	
	Log("[setPlayOrDieTimer] setting timer...\n");
	
	mPlayOrDieTimer = [NSTimer scheduledTimerWithTimeInterval: 8.0
													   target: self
													 selector: @selector(playOrDieTimerFired:)
													 userInfo: self
													  repeats: NO];
	if( mPlayOrDieTimer )
	{
		[mPlayOrDieTimer retain];
	}
	
	return kIOReturnSuccess;
}

//===================================================================================================
- (IOReturn) resetPlayOrDieTimer
{
	if( mPlayOrDieTimer )
	{
		[mPlayOrDieTimer invalidate];
		[mPlayOrDieTimer release];
		mPlayOrDieTimer = nil;
	}
	
	Log("[resetPlayOrDieTimer] timer is reset\n");
	
	return kIOReturnSuccess;
}

//===================================================================================================
- (void) playOrDieTimerFired:(id)inTimer
{
	Log("[PlayOrDieTimer] No sound, Im outta here!\n");
	
	[self shutdown];
}

#pragma mark -
//===========================================================================================================================
//
//	Here are the facts: There are devices on the market that just don't play well with others.  If you want to work with them
// you'll need to make them an offer they cannot refuse.  In our case, we'll just open the SCO channel on them whether they
// like it or not if they decide to be lazy and not tell us when we can.
//
//===========================================================================================================================
- (void) resetServiceLevelTimer
{
	if( mServiceLevelTimer )
	{
		[mServiceLevelTimer invalidate];
		[mServiceLevelTimer release];
		mServiceLevelTimer = nil;
	}
}


//===========================================================================================================================
//
//===========================================================================================================================
- (void) setServiceLevelTimer
{
	[self resetServiceLevelTimer];
	
	mServiceLevelTimer = [NSTimer scheduledTimerWithTimeInterval: kDefaultServiceLevelTimeout
														  target: self
														selector: @selector(handleServiceLevelTimeout:)
														userInfo: nil
														 repeats: NO];
	if( mServiceLevelTimer )
	{
		[mServiceLevelTimer retain];
	}
}


//===========================================================================================================================
//
//===========================================================================================================================
- (void) handleServiceLevelTimeout:(id)sender
{
	[self resetServiceLevelTimer];
	
	Log("[SL Timeout] entry!\n");
	
	if( mHandsFreeGateway )
	{
		[mHandsFreeGateway openSCOConnection];
	}
	else if( mHeadsetDevice )
	{
		[mHeadsetDevice openSCOConnection];
	}
}



#pragma mark -
#pragma mark === SCO Headset Stuff ===
//====================================================================================================================
//  Device Opened
//====================================================================================================================
- (void) audioDevice:(id)device deviceConnectionOpened:(IOReturn)status
{	
	Log("[Device Open] result: 0x%x\n", status);
	
	mIsDoneLaunching = TRUE;
	
	[self resetFinishConnectionTimer];
	[self resetPlayOrDieTimer];
	[self setPlayOrDieTimer];
	
	if( status != kIOReturnSuccess )
	{
		[self shutdownWithError: kBTAudioError];
	}
}

//====================================================================================================================
//  Device Closed
//====================================================================================================================
- (void) audioDevice:(id)device deviceConnectionClosed:(IOReturn)status
{
	Log("[Device Closed] result: 0x%x\n", status);

	mIsDoneLaunching = TRUE;

	if( status != kIOReturnSuccess )
	{
		[self shutdownWithError: kBTAudioError];
	}
	else
	{
		[self shutdown];
	}
}


#pragma mark -
//====================================================================================================================
//  RFCOMM Opened
//====================================================================================================================
- (void) audioDevice:(id)device rfcommChannelOpened:(IOReturn)status
{
	Log("[RFCOMM Open] channel open: 0x%x\n", status);
	
	mIsDoneLaunching = TRUE;
	
	[self resetPlayOrDieTimer];
	
	if( status != kIOReturnSuccess )
	{
		if( status == kIOReturnNotOpen )
		{
			Log("[RFCOMM Open] lost our device, due to power management?\n");
		}
		
		[self shutdown];	
	}
}



#pragma mark -
//====================================================================================================================
//  SL Opened
//====================================================================================================================
- (void) audioDevice:(id)device serviceLevelConnectionOpened:(IOReturn)status
{
	Log("[SL Connection Opened] status: 0x%x\n", status);
	
	[self resetPlayOrDieTimer];

	if( status == kIOReturnSuccess )
	{
	}
	else
	{
		[self shutdownWithError: kBTAudioError];
	}
}

//====================================================================================================================
//   SL Connection Complete
//====================================================================================================================
- (void) audioDevice:(id)device serviceLevelConnectionComplete:(IOReturn)status
{
	//kIOReturnSuccess;
	Log("[SL Connection Complete] service connection complete 0x%x\n", status);
	
	[self resetServiceLevelTimer];

	if( status != kIOReturnSuccess )
	{
		[self shutdownWithError: kBTAudioError];
	}	
}


//====================================================================================================================
//  SL Closed
//====================================================================================================================
- (void) audioDevice:(id)device serviceLevelConnectionClosed:(IOReturn)status
{
	Log("[SL Connection Closed] status: 0x%x\n", status);
	
	if( status == kIOReturnSuccess )
	{
		[self shutdown];
	}
	else
	{
		[self shutdownWithError: kBTAudioError];
	}
}




#pragma mark -
//====================================================================================================================
// Open
//====================================================================================================================
- (void) audioDevice:(id)device scoConnectionOpened:(IOReturn)status
{
	Log("[SCO Connection Opened] status: 0x%x\n", status);
	
	[self resetServiceLevelTimer];
	[self resetPlayOrDieTimer];
	
	if( status == kIOReturnSuccess )
	{
	}
	else
	{
		[self shutdownWithError: kBTAudioError];
	}
}

//====================================================================================================================
//   Closed
//====================================================================================================================
- (void) audioDevice:(id)device scoConnectionClosed:(IOReturn)status
{
	Log("[SCO Connection Closed] status: 0x%x\n", status);
	
	if( status == kIOReturnSuccess )
	{
		[self shutdown];
	}
	else
	{
		[self shutdownWithError: kBTAudioError];
	}
}



@end



