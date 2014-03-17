/*
	File:		MyWindow.m
	Contains:	Sample code for using the OBEXSession API's available in the IOBluetooth Framework.
				This sample uses the C API available in the framework; there is also an objective-c
				API as well, see other OBEX samples on how to use it.
	Copyright (c) 2002-2005 by Apple Computer, Inc., all rights reserved.
*/
/*
	IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc. ("Apple") in
	consideration of your agreement to the following terms, and your use, installation, 
	modification or redistribution of this Apple software constitutes acceptance of these 
	terms.  If you do not agree with these terms, please do not use, install, modify or 
	redistribute this Apple software.
	
	In consideration of your agreement to abide by the following terms, and subject to these 
	terms, Apple grants you a personal, non-exclusive license, under AppleÕs copyrights in 
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


#import "MyWindow.h"

@implementation MyWindow

#pragma mark -
#pragma mark === IB Actions ===

//===========================================================================================================================
//	PutButtonClicked
//===========================================================================================================================

- (IBAction)PutButtonClicked:(id)sender
{
	IOReturn					status;
	BluetoothDeviceAddress		deviceAddress;

	[self releaseDataItems];
	
	// Pre-flight the data.
	
	if( ([[TextFieldFirstName stringValue] length] + [[TextFieldLastName stringValue] length]) > 18 )
	{
		[TextFieldStatus setStringValue:[NSString stringWithFormat:@"Name too long."]];
		return;
	}

	// Alter UI.

	[ButtonPut setEnabled:FALSE];
	[TextFieldStatus setStringValue:[NSString stringWithFormat:@""]];

	// Get a session.

	if( mOBEXSessionRef != NULL )
	{
		status = OBEXSessionDelete( mOBEXSessionRef );
		if( status != kIOReturnSuccess )
		{
			[TextFieldStatus setStringValue:[NSString stringWithFormat:@"Session delete failed. %x", status ]];
		}
		mOBEXSessionRef = NULL;
		[ButtonPut setEnabled:TRUE];
	}
	
	// Create session with browser or manually.
	
	status = IOBluetoothNSStringToDeviceAddress( [TextFieldDeviceAddress stringValue], &deviceAddress );
	if( status == kIOReturnSuccess )
	{
		IOBluetoothDeviceRef		deviceRef;
		BluetoothRFCOMMChannelID	channelID;

		[TextFieldStatus setStringValue:@"Opening connection..."];
	
		channelID = [TextFieldChannelID intValue];

		// Create session ref, using service we got from the browser.
	
		deviceRef = IOBluetoothDeviceCreateWithAddress( &deviceAddress );
		if( !deviceRef )
		{
			[TextFieldStatus setStringValue:[NSString stringWithFormat:@"An error occurred(1): %x", status ]];
			[ButtonPut setEnabled:TRUE];
			return;
		}
		
		status = IOBluetoothOBEXSessionCreateWithIOBluetoothDeviceRefAndChannelNumber( deviceRef, channelID, &mOBEXSessionRef );
		if( status != kIOReturnSuccess )
		{
			[TextFieldStatus setStringValue:[NSString stringWithFormat:@"An error occurred(1.5): %x", status ]];
			[ButtonPut setEnabled:TRUE];
			return;
		}
	}
	else
	{
		IOBluetoothSDPServiceRecordRef serviceRecordRef;
		
		// Get browser to get a service ref.

		status = IOBluetoothServiceBrowserControllerBrowseDevices( &serviceRecordRef, kIOBluetoothServiceBrowserControllerOptionsNone );
		if( status == kIOBluetoothUIUserCanceledErr )
		{
			[TextFieldStatus setStringValue:[NSString stringWithFormat:@"Canceled by user."]];
			[ButtonPut setEnabled:TRUE];
			return;
		}
		else if( status != kIOReturnSuccess )
		{
			[TextFieldStatus setStringValue:[NSString stringWithFormat:@"An error occurred(2): %x", status ]];
			[ButtonPut setEnabled:TRUE];
			return;
		}

		status = IOBluetoothOBEXSessionCreateWithIOBluetoothSDPServiceRecordRef( serviceRecordRef, &mOBEXSessionRef );
		if( status != kIOReturnSuccess )
		{
			[TextFieldStatus setStringValue:[NSString stringWithFormat:@"An error occurred(3): %x", status ]];
			[ButtonPut setEnabled:TRUE];
			return;
		}
	}

	// Make an OBEX connection.
	
	status = OBEXSessionConnect(	mOBEXSessionRef,
									kOBEXConnectFlagNone,		// connect flags
									350,						// max packet size
									NULL,						// opt headers
									0,							// opt header size
									ConnectedCallback,			// callback
									self );						// refcon
	if( status != kIOReturnSuccess )
	{
		IOReturn status2 = OBEXSessionDelete( mOBEXSessionRef );
		if( status2 != kIOReturnSuccess )
		{
			[TextFieldStatus setStringValue:[NSString stringWithFormat:@"Session delete failed. %x", status2 ]];
		}
		mOBEXSessionRef = NULL;
		[TextFieldStatus setStringValue:[NSString stringWithFormat:@"An error occurred(4): %x", status ]];
		[ButtonPut setEnabled:TRUE];
	}
}

#pragma mark -
#pragma mark === Support stuff ===

//===========================================================================================================================
//	CreateVCardFromTextFieldData
//===========================================================================================================================

- (NSData*)CreateVCardFromTextFieldData
{
	return( (NSData*) OBEXCreateVCard(	[[TextFieldFirstName stringValue] UTF8String],
									[[TextFieldFirstName stringValue] length],
									[[TextFieldLastName stringValue] UTF8String],
									[[TextFieldLastName stringValue] length],
									NULL,
									0,
									0,
									0,
									[[TextFieldHomePhone stringValue] UTF8String],
									[[TextFieldHomePhone stringValue] length],
									[[TextFieldWork stringValue] UTF8String],
									[[TextFieldWork stringValue] length],
									[[TextFieldMobilePhone stringValue] UTF8String],
									[[TextFieldMobilePhone stringValue] length],
									[[TextFieldFaxPhone stringValue] UTF8String],
									[[TextFieldFaxPhone stringValue] length],
									[[TextFieldEMailAddress stringValue] UTF8String],
									[[TextFieldEMailAddress stringValue] length],
									NULL,
									0,
									[[TextFieldOrganization stringValue] UTF8String],
									[[TextFieldOrganization stringValue] length],
									NULL,
									0,
									[[TextFieldTitle stringValue] UTF8String],
									[[TextFieldTitle stringValue] length],
									NULL,
									0		) );
/*								
		return( (NSData*) OBEXCreateVCard(	[[TextFieldFirstName stringValue] UTF8String],
								[[TextFieldFirstName stringValue] length],
								[[TextFieldLastName stringValue] UTF8String],
								[[TextFieldLastName stringValue] length],
								kCharsetStringISO88591,
								strlen( kCharsetStringISO88591 ),
								0,
								0,
								[[TextFieldHomePhone stringValue] UTF8String],
								[[TextFieldHomePhone stringValue] length],
								[[TextFieldWork stringValue] UTF8String],
								[[TextFieldWork stringValue] length],
								[[TextFieldMobilePhone stringValue] UTF8String],
								[[TextFieldMobilePhone stringValue] length],
								[[TextFieldFaxPhone stringValue] UTF8String],
								[[TextFieldFaxPhone stringValue] length],
								[[TextFieldEMailAddress stringValue] UTF8String],
								[[TextFieldEMailAddress stringValue] length],
								kCharsetStringISO88591,
								strlen( kCharsetStringISO88591 ),
								[[TextFieldOrganization stringValue] UTF8String],
								[[TextFieldOrganization stringValue] length],
								kCharsetStringISO88591,
								strlen( kCharsetStringISO88591 ),
								[[TextFieldTitle stringValue] UTF8String],
								[[TextFieldTitle stringValue] length],
								kCharsetStringISO88591,
								strlen( kCharsetStringISO88591	)		) );
*/
}

//===========================================================================================================================
//	releaseDataItems
//===========================================================================================================================

- (void)releaseDataItems
{
	if( mHeadersData )
	{
		CFRelease( mHeadersData );
		mHeadersData = NULL;
	}
	
	if( mHeadersDictionary )
	{
		CFRelease( mHeadersDictionary );
		mHeadersDictionary = NULL;
	}
}
	
#pragma mark -
#pragma mark === Preferences handling ===

//===========================================================================================================================
//	saveUIState
//===========================================================================================================================

- (void)saveUIState
{
    NSUserDefaults		*userDefaults;
    
    userDefaults = [NSUserDefaults standardUserDefaults];
    [userDefaults setObject:[TextFieldDeviceAddress stringValue] forKey:kDeviceAddressKey];
    [userDefaults setObject:[TextFieldChannelID stringValue] forKey:kChannelIDKey];
    
    [userDefaults synchronize];
    
}

//===========================================================================================================================
//	restoreUIState
//===========================================================================================================================

- (void)restoreUIState
{
    NSUserDefaults		*userDefaults;
    NSString			*tempString;

    userDefaults = [NSUserDefaults standardUserDefaults];

    tempString = [userDefaults objectForKey:kDeviceAddressKey];
    if(tempString)
        [TextFieldDeviceAddress setStringValue:tempString];

    tempString = [userDefaults objectForKey:kChannelIDKey];
    if(tempString)
        [TextFieldChannelID setStringValue:tempString];

        
}

//===========================================================================================================================
//	awakeFromNib
//===========================================================================================================================

- (void)awakeFromNib
{
    [NSApp	setDelegate:self];
    [self restoreUIState];
}

//===========================================================================================================================
//	applicationWillTerminate
//===========================================================================================================================

- (void)applicationWillTerminate:(NSNotification *)sender
{
    [self saveUIState];
}

@end

#pragma mark -
#pragma mark === OBEX C API Callbacks ===

//===========================================================================================================================
//	ConnectedCallback
//===========================================================================================================================

void ConnectedCallback( const OBEXSessionEvent * inEvent )
{
	MyWindow*		window = (MyWindow*) inEvent->refCon;
	OBEXError		status;
	
	if( inEvent->u.connectCommandResponseData.serverResponseOpCode == kOBEXResponseCodeSuccessWithFinalBit )
	{
		// We have successfully connected to the server.
	
		NSData*					vCardData;
		OBEXError 				status;
		OBEXMaxPacketLength		spaceAvailable;
		
		[window->TextFieldStatus setStringValue:@"Connected. Sending card..."];

		window->mHeadersDictionary = CFDictionaryCreateMutable(	kCFAllocatorDefault,
																0,
																&kCFTypeDictionaryKeyCallBacks,
																&kCFTypeDictionaryValueCallBacks );
		if( !window->mHeadersDictionary )
		{
			goto errorExit;
		}
				
		OBEXAddNameHeader( CFSTR( "idiot.vcf" ), window->mHeadersDictionary );
		OBEXAddTypeHeader( CFSTR( "text/x-vCard"), window->mHeadersDictionary );

		window->mHeadersData = OBEXHeadersToBytes( (CFDictionaryRef) window->mHeadersDictionary );
		if( !window->mHeadersData )
		{
			goto errorExit;
		}
		
		vCardData = [window CreateVCardFromTextFieldData];
		if( !vCardData )
		{
			goto errorExit;
		}

		// Make sure we have enough space in our command payload for our vCard information. If we don't, we'd
		// need to send the vCard across two Puts. But we'll just error out instead.
		
		status = OBEXSessionGetAvailableCommandPayloadLength( window->mOBEXSessionRef, kOBEXOpCodePut, &spaceAvailable );
		if( status != kOBEXSuccess ) goto errorExit;

		if( spaceAvailable < (CFDataGetLength( window->mHeadersData ) + [vCardData length]) )
		{
			// uh-oh, too much data. Bail.
			[window->TextFieldStatus setStringValue:@"vCard too big for this lame app!"];
			NSLog( @"vCard too big for this lame app!" );
			goto errorExit;
		}

		// Send the Put command.
	
		status = OBEXSessionPut(	window->mOBEXSessionRef,
									TRUE,
									(void*) CFDataGetBytePtr( window->mHeadersData ),
									CFDataGetLength( window->mHeadersData ),
									(void*) [vCardData bytes],
									[vCardData length],
									PutCallback,
									window );
		if( status != kIOReturnSuccess )
		{
			[window->TextFieldStatus setStringValue:[NSString stringWithFormat:@"OBEXSessionPut failed. Error = %x.", status ]];
			goto errorExit;
		}
	}

	[window releaseDataItems];

	return;
	
errorExit:

	[window releaseDataItems];
	status = OBEXSessionDisconnect( window->mOBEXSessionRef, NULL, 0, DisconnectedCallback, window );
	if( status != kIOReturnSuccess )
	{
		[window->TextFieldStatus setStringValue:[NSString stringWithFormat:@"OBEXSessionDisconnect failed. %x", status ]];
	}
}

//===========================================================================================================================
//	PutCallback
//===========================================================================================================================

void PutCallback( const OBEXSessionEvent * inEvent )
{
	IOReturn	status;
	
	NSLog( @"[PutCallback] Entry" );
	
	// Put called back.

	if( inEvent->refCon )
	{
		MyWindow* window = (MyWindow*) inEvent->refCon;
		
		// Do a disconnect command.
		
		status = OBEXSessionDisconnect( window->mOBEXSessionRef, NULL, 0, DisconnectedCallback, window );
		if( status != kIOReturnSuccess )
		{
			[window->TextFieldStatus setStringValue:[NSString stringWithFormat:@"OBEXSessionDisconnect failed. %x", status ]];
		}
	}
}

//===========================================================================================================================
//	DisconnectedCallback
//===========================================================================================================================

void DisconnectedCallback( const OBEXSessionEvent * inEvent )
{
	NSLog( @"[DisconnectedCallback] Entry" );

	if( inEvent->refCon )
	{
		MyWindow* window = (MyWindow*) inEvent->refCon;

		[window->TextFieldStatus setStringValue:@"Idle."];
		[window->ButtonPut setEnabled:TRUE];
	
		if( window->mOBEXSessionRef != NULL )
		{
			OBEXError status = OBEXSessionDelete( window->mOBEXSessionRef );
			if( status != kIOReturnSuccess )
			{
				[window->TextFieldStatus setStringValue:[NSString stringWithFormat:@"Session delete failed. %x", status ]];
			}
			window->mOBEXSessionRef = NULL;
		}
	}
}
