/*
	File: ChatBluetoothServerInterface.m
	Constains: Bluetooth Server Sample
	Author: Marco Pontil

	Copyright (c) 2002 by Apple Computer, Inc., all rights reserved.
*/
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

#import "ChatBluetoothServerInterface.h"

#import <IOBluetooth/objc/IOBluetoothDevice.h>

@implementation ChatBluetoothServerInterface

// Connection Method:
// returns TRUE if the connection was successful:
- (BOOL)publishService
{
    BOOL				returnValue = FALSE;
    NSString			*dictionaryPath = nil;
	NSString			*serviceName = nil;
    NSMutableDictionary	*sdpEntries = nil;
    
    // Builds a string with the service name we wish to offer
    serviceName = [NSString stringWithFormat:@"%@ Chat Server", [self localDeviceName]];
    
    // Get the path for the dictonary we wish to publish.
    dictionaryPath = [[NSBundle mainBundle] pathForResource:@"SerialPortDictionary" ofType:@"plist"];

    if ( ( dictionaryPath != nil ) && ( serviceName != nil ) ) 
    {
        // Loads the dictionary from the path:
        sdpEntries = [NSMutableDictionary dictionaryWithContentsOfFile:dictionaryPath];
        
        if ( sdpEntries != nil )
        {
            IOBluetoothSDPServiceRecordRef	serviceRecordRef;
            
            [sdpEntries setObject:serviceName forKey:@"0100 - ServiceName*"];
        
            // Add SDP dictionary, the rfcomm channel assigned to this service comes back in mServerChannelID. While mServerHandle
            // is going to be the record handle assigned to this service.
            if (IOBluetoothAddServiceDict( (CFDictionaryRef) sdpEntries, &serviceRecordRef ) == kIOReturnSuccess)
            {
                IOBluetoothSDPServiceRecord *serviceRecord;
                
                serviceRecord = [IOBluetoothSDPServiceRecord withSDPServiceRecordRef:serviceRecordRef];
                
                [serviceRecord getRFCOMMChannelID:&mServerChannelID];
                [serviceRecord getServiceRecordHandle:&mServerHandle];
				
				IOBluetoothObjectRelease( serviceRecordRef );
                
                // Register a notification so we get notified when an incoming RFCOMM channel is opened
				// to the channel assigned to our chat service.
                
                mIncomingChannelNotification = [IOBluetoothRFCOMMChannel registerForChannelOpenNotifications:self selector:@selector(newRFCOMMChannelOpened:channel:) withChannelID:mServerChannelID direction:kIOBluetoothUserNotificationChannelDirectionIncoming];
                
                returnValue = TRUE;
            }
        }
    }
    
    return returnValue;
}

// Stops Providing chat services
// Removes the published services from the SDP dictionary:
// If a connection is in progress it will not interrupt it.
- (void)stopProvidingChatServices
{
    if ( mServerHandle != 0 )
    {
        // Removes the service:
		IOBluetoothRemoveServiceWithRecordHandle( mServerHandle );
    }
    
    // Unregisters the notification:
    if ( mIncomingChannelNotification != nil )
	{
        [mIncomingChannelNotification unregister];
		mIncomingChannelNotification = nil;
	}
	
	mServerChannelID = 0;
}

// Disconnection:
// closes the channel:
- (void)disconnectFromClient
{
    if ( mRFCOMMChannel != nil )
    {
        IOBluetoothDevice *device = [mRFCOMMChannel getDevice];
        
        // And closes the channel:
        [mRFCOMMChannel closeChannel];
                
        // We do not need the channel anymore:
        [mRFCOMMChannel release];
        mRFCOMMChannel = nil;
        
        // And closes the connection with the device:
        [device closeConnection];
    }
}

// New RFCOMM channel show up:
// A new RFCOMM channel shows up we have to make sure it is the one we are waiting for:
-(void) newRFCOMMChannelOpened:(IOBluetoothUserNotification *)inNotification channel:(IOBluetoothRFCOMMChannel *)newChannel
{
    // Make sure the channel is an incoming channel on the right channel ID.
	// This isn't strictly necessary since we only registered a notification for this case,
	// but it can't hurt to double-check.
    if ( ( newChannel != nil ) && [newChannel isIncoming] && ( [newChannel getChannelID] == mServerChannelID ) )
    {
		mRFCOMMChannel = newChannel;
		
		// Retains the channel
		[mRFCOMMChannel retain];
				
		// Set self as the channel's delegate: THIS IS THE VERY FIRST THING TO DO FOR A SERVER !!!!
		if ( [mRFCOMMChannel setDelegate:self] == kIOReturnSuccess )
		{                
			// stop providing the services (this example only handles one chat connection at a time - but
			// there's no reason a well written app can't handle any number of connections)
			[self stopProvidingChatServices];

			// And notify our UI client that we have a new chat connection:
			[mConnectionTarget performSelector:mHandleRemoteConnectionSelector];
		}
		else
		{
			// The setDelgate: call failed. This is catastrophic for a server
			// Releases the channel:
			[mRFCOMMChannel release];
			mRFCOMMChannel = nil;
		}
    }
}

// Send Data method
// returns TRUE if all the data was sent:
- (BOOL)sendData:(void *)buffer length:(UInt32)length;
{
    if ( mRFCOMMChannel != nil )
    {
        UInt32				numBytesRemaining;
        IOReturn			result;
		BluetoothRFCOMMMTU	rfcommChannelMTU;
		
		numBytesRemaining = length;
		result = kIOReturnSuccess;
		
		// Get the RFCOMM Channel's MTU.  Each write can only contain up to the MTU size
		// number of bytes.
		rfcommChannelMTU = [mRFCOMMChannel getMTU];
		
		// Loop through the data until we have no more to send.
		while ( ( result == kIOReturnSuccess ) && ( numBytesRemaining > 0 ) )
		{
			// finds how many bytes I can send:
			UInt32 numBytesToSend = ( ( numBytesRemaining > rfcommChannelMTU ) ? rfcommChannelMTU :  numBytesRemaining );
			
			// This method won't return until the buffer has been passed to the Bluetooth hardware to be sent to the remote device.
			// Alternatively, the asynchronous version of this method could be used which would queue up the buffer and return immediately.
			result = [mRFCOMMChannel writeSync:buffer length:numBytesToSend];
			
			// Updates the position in the buffer:
			numBytesRemaining -= numBytesToSend;
			buffer += numBytesToSend;
		}
		
        // We are successful only if all the data was sent:
        if ( ( numBytesRemaining == 0 ) && ( result == kIOReturnSuccess ) )
		{
            return TRUE;
		}
    }

    return FALSE;
}

// Returns the name of the device we are connected to
// returns nil if not connection:
- (NSString *)remoteDeviceName
{
    NSString *deviceName = nil;
    
    if ( mRFCOMMChannel != nil )
    {
        // Gets the device:
        IOBluetoothDevice *device = [mRFCOMMChannel getDevice];
        
        // .. and its name:
        deviceName = [device getName];
    }
    
    return deviceName;
}


// Implementation of delegate calls (see IOBluetoothRFCOMMChannel.h) Only the basic ones:
- (void)rfcommChannelData:(IOBluetoothRFCOMMChannel*)rfcommChannel data:(void *)dataPointer length:(size_t)dataLength;
{
	[mNewDataTarget performSelector:mHandleNewDataSelector withObject:[NSData dataWithBytes:dataPointer length:dataLength]];
}

- (void)rfcommChannelClosed:(IOBluetoothRFCOMMChannel*)rfcommChannel;
{
	[mDisconnectionTarget performSelector:mHandleRemoteDisconnectionSelector];
}

// Registers selector for incoming data:
// tells to this class to call myTarget and myTargetAction when new data shows up:
- (void)registerForNewData:(id)myTarget action:(SEL)actionMethod
{
	mHandleNewDataSelector = actionMethod;
	mNewDataTarget = myTarget;
}

// Registers selector for disconnection:
// tells to this class to call myTarget and myTargetAction when the channel disconnects:
- (void)registerForTermination:(id)myTarget action:(SEL)actionMethod
{
	mHandleRemoteDisconnectionSelector = actionMethod;
	mDisconnectionTarget = myTarget;
}

// Registers selector for a successful completed connection:
// tells to this class to call myTarget and myTargetAction when the channel obtains a connection:
- (void)registerForNewConnection:(id)myTarget action:(SEL)actionMethod
{
	mHandleRemoteConnectionSelector = actionMethod;
	mConnectionTarget = myTarget;
}

// Returns the name of the local bluetooth device
- (NSString *)localDeviceName
{
    BluetoothDeviceName localDeviceName;

    if (IOBluetoothLocalDeviceReadName( localDeviceName, NULL, NULL, NULL ) == kIOReturnSuccess)
    {
        return [NSString stringWithUTF8String:(const char*)localDeviceName];
    }

    return nil;
}

@end
