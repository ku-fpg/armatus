//
//  ChatBluetoothInterface_ObjC.m
//  IOBluetoothFamily
//
//  Created by Eric Brown on Fri Apr 18 2003.
//  Copyright (c) 2003 __MyCompanyName__. All rights reserved.
//

// Only compile this file if we are building the ObjC version of the app.

#ifndef USE_C_API

#import "ChatBluetoothInterface_ObjC.h"

#import <IOBluetooth/objc/IOBluetoothDevice.h>
#import <IOBluetooth/objc/IOBluetoothRFCOMMChannel.h>
#import <IOBluetooth/objc/IOBluetoothSDPServiceRecord.h>
#import <IOBluetooth/objc/IOBluetoothSDPUUID.h>

#import <IOBluetoothUI/objc/IOBluetoothDeviceSelectorController.h>

@implementation ChatBluetoothInterface_ObjC

// Connection Method:
// returns TRUE if the connection was successful:
- (BOOL)connectToServer
{
    BOOL								returnValue = FALSE;
    IOBluetoothDeviceSelectorController	*deviceSelector;
	IOBluetoothSDPUUID					*chatServiceUUID;
	NSArray								*deviceArray;
	IOBluetoothDevice					*selectedDevice;
	IOBluetoothSDPServiceRecord			*chatServiceRecord;
	UInt8								rfcommChannelID;
	IOReturn							status;
	
    // The device selector will provide UI to the end user to find a remote device
    deviceSelector = [IOBluetoothDeviceSelectorController deviceSelector];
	
	if ( deviceSelector == nil )
	{
		NSLog( @"Error - unable to allocate IOBluetoothDeviceSelectorController.\n" );
		goto exit;
	}
		
	// Create an IOBluetoothSDPUUID object for the chat service UUID
	chatServiceUUID = [IOBluetoothSDPUUID uuidWithBytes:gExampleChatServiceClassUUID length:16];

	// Tell the device selector what service we are interested in.
	// It will only allow the user to select devices that have that service.
	[deviceSelector addAllowedUUID:chatServiceUUID];
	
	// Run the device selector modal.  This won't return until the user has selected a device and the device has
	// been validated to contain the specified service or the user has hit the cancel button.
	if ( [deviceSelector runModal] != kIOBluetoothUISuccess )
	{
		NSLog( @"User has cancelled the device selection.\n" );
		goto exit;
	}
		
	// Get the list of devices the user has selected.
	// By default, only one device is allowed to be selected.
	deviceArray = [deviceSelector getResults];
	
	if ( ( deviceArray == nil ) || ( [deviceArray count] == 0 ) )
	{
		NSLog( @"Error - no selected device.  ***This should never happen.***\n" );
		goto exit;
	}
		
	// Since only one device was allowed to be selected, we only care about the 
	// first entry in the array.
	selectedDevice = [deviceArray objectAtIndex:0];
	
	// Get the chat service record from the device the user has selected.
	// We can assume that the device selector performed an SDP query, so we can
	// just get the service record from the device's cache.
	chatServiceRecord = [selectedDevice getServiceRecordForUUID:chatServiceUUID];
	
	if ( chatServiceRecord == nil )
	{
		NSLog( @"Error - no chat service in selected device.  ***This should never happen.***\n" );
		goto exit;
	}

	// To connect we need a device to connect and an RFCOMM channel ID to open on the device:
	status = [chatServiceRecord getRFCOMMChannelID:&rfcommChannelID];
	
	// Check to make sure the service record actually had an RFCOMM channel ID
	if ( status != kIOReturnSuccess )
	{
		NSLog( @"Error: 0x%lx getting RFCOMM channel ID from service.\n", status );
		goto exit;
	}
	
	// The service record contains all the useful information about the service the user selected
	// Just for fun we log its name:
	NSLog( @"Service selected '%@' - RFCOMM Channel ID = %d\n", [chatServiceRecord getServiceName], rfcommChannelID );
	
	// Before we can open the RFCOMM channel, we need to open a connection to the device.
	// The openRFCOMMChannel... API probably should do this for us, but for now we have to
	// do it manually.
	// This -openConnection call is synchronous, but there is also an asynchronous version -openConnection:
	status = [selectedDevice openConnection];
	
	if ( status != kIOReturnSuccess )
	{
		NSLog( @"Error: 0x%lx opening connection to device.\n", status );
		goto exit;
	}
	
	// Open the RFCOMM channel on the new device connection
	status = [selectedDevice openRFCOMMChannelSync:&mRFCOMMChannel withChannelID:rfcommChannelID delegate:self];
	if ( ( status == kIOReturnSuccess ) && ( mRFCOMMChannel != nil ) )
	{
		// Retains the channel
		[mRFCOMMChannel retain];

		// And the return value is TRUE !!
		returnValue = TRUE;
	}
	else
	{
		NSLog( @"Error: 0x%lx - unable to open RFCOMM channel.\n", status );
	}
	
exit:

    return returnValue;
}

// Disconnection:
// closes the channel:
- (void)disconnectFromServer
{
    if ( mRFCOMMChannel != nil )
    {
        IOBluetoothDevice *device = [mRFCOMMChannel getDevice];
        
        // This will close the RFCOMM channel and start an inactivity timer to close the baseband connection if no
		// other channels (L2CAP or RFCOMM) are open.
        [mRFCOMMChannel closeChannel];
                
        // Release the channel object since we are done with it and it isn't useful anymore.
        [mRFCOMMChannel release];
        mRFCOMMChannel = nil;
        
        // This signals to the system that we are done with the baseband connection to the device.  If no other
		// channels are open, it will immediately close the baseband connection.
        [device closeConnection];
    }
}

// Send Data method
// returns TRUE if all the data was sent:
- (BOOL)sendData:(void*)buffer length:(UInt32)length
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
        IOBluetoothDevice	*device = [mRFCOMMChannel getDevice];
        
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
	[mRemoteDisconnectionTarget performSelector:mHandleRemoteDisconnectionSelector];
}

@end

#endif // !USE_C_API

