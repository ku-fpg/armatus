/*
	File: ChatBluetoothInterface_C.m
	Constains: Bluetooth sample
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
	terms, Apple grants you a personal, non-exclusive license, under Apple‚Äôs copyrights in 
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

// Only compile this file if we are building the C version of the sample code

#ifdef USE_C_API

#import "ChatBluetoothInterface_C.h"

#include <IOBluetoothUI/IOBluetoothUIUserLib.h>

// "C" version of the code:

// Proto for the "C" callback:
void rfcommEventListener (IOBluetoothRFCOMMChannelRef rfcommChannel, void *refCon, IOBluetoothRFCOMMChannelEvent *event);

@implementation ChatBluetoothInterface_C

// Connection Method:
// returns TRUE if the connection was successful:
- (BOOL)connectToServer
{
    BOOL									returnValue = FALSE;
    IOBluetoothSDPServiceRecordRef 			serviceRecord;
    CFArrayRef								serviceArray;
    IOBluetoothSDPUUIDRef					chatUUID;
    IOBluetoothServiceBrowserControllerRef	serviceBrowser;
	IOReturn								result;
    
	// NOTE: Ideally, we'd use the IOBluetoothDeviceSelectorController instead of the service selector so that
	// the user doesn't have to know anything about the service that we are using.  However currently the API
	// on the device selector to add allowed UUIDs that it uses to validate that the selected device has the proper
	// service only exists in ObjC form.  There will be C versions in a future release as well as support for using
	// the C UI API from a Carbon application.  For now, this example will use the service selector, but see the
	// ObjC version of the example for the more appropriate way to provide UI to the user.
	
    // First, we create a service browser controller which will allow us to put up a panel to allow the user
	// to select a device and service.
    serviceBrowser = IOBluetoothServiceBrowserControllerCreate( kIOBluetoothServiceBrowserControllerOptionsNone );

    // Now we create an array with all the UUID we are interested in (in this case only our custom one):
    chatUUID = IOBluetoothSDPUUIDCreateWithBytes(gExampleChatServiceClassUUID, 16);
    
    serviceArray = CFMakeCollectable( CFArrayCreate( kCFAllocatorDefault, (const void **)&chatUUID, 1, &kCFTypeArrayCallBacks ) );

	// This call will return kIOReturnSuccess if the user has successfully selected a device and service that matches the 
	// specified serviceArray.
	result = IOBluetoothServiceBrowserControllerDiscoverWithDeviceAttributes(serviceBrowser, &serviceRecord, NULL, serviceArray);
	
    if ( ( result == kIOReturnSuccess ) && 
        ( serviceRecord != NULL ) )
    {
        UInt8					rfcommChannelID;

        // To connect we need a device to connect and an RFCOMM channel ID to open on the device:
		result = IOBluetoothSDPServiceRecordGetRFCOMMChannelID(serviceRecord, &rfcommChannelID);
		
		if ( result == kIOReturnSuccess )
		{
			IOBluetoothDeviceRef 	device;
			
			// The service record contains all the useful information about the service the user selected
			// Just for fun we log its name and RFCOMM Channel ID
			NSLog( @"Service selected '%@' - RFCOMM Channel ID = %d\n", (NSString*)IOBluetoothSDPServiceRecordGetServiceName( serviceRecord ), rfcommChannelID );
			
			// Get the device from the service record
			device = IOBluetoothSDPServiceRecordGetDevice( serviceRecord );
			
			// We must first open the baseband connection to the device before we can open the RFCOMM channel.  The RFCOMM Channel
			// open API should probably do this automatically, but for now we have to do it manually.
			if ( ( device != NULL ) && ( IOBluetoothDeviceOpenConnection(device, NULL, NULL) == kIOReturnSuccess ) )
			{
	
				// Open channel and registers our callback for RFCOMM Events:
				result = IOBluetoothDeviceOpenRFCOMMChannelSync(device ,&mRFCOMMChannelRef, rfcommChannelID, rfcommEventListener, (void*)self);
				
				if ( result == kIOReturnSuccess )
				{
					returnValue = TRUE;
				}
			}
		}
    }
    
    [(id)serviceArray release];
    IOBluetoothObjectRelease(chatUUID);
    IOBluetoothObjectRelease(serviceBrowser);
    
    return returnValue;
}

// Disconnection:
// closes the channel:
- (void)disconnectFromServer
{
    if ( mRFCOMMChannelRef != NULL )
    {
        // This will close the RFCOMM channel and start an inactivity timer that will close the baseband connection if no other
		// channels (L2CAP or RFCOMM) are open after a set period of time.
        IOBluetoothRFCOMMChannelCloseChannel( mRFCOMMChannelRef );
        
        // This close connection call signals to the system that we are done with the baseband connection.  If no other
		// channels are open, it will immediately close the baseband connection.
        IOBluetoothDeviceCloseConnection( IOBluetoothRFCOMMChannelGetDevice( mRFCOMMChannelRef ) );
        
        // Since we are done with the RFCOMM channel ref, we need to release it.
		IOBluetoothObjectRelease( mRFCOMMChannelRef );
        
        mRFCOMMChannelRef = NULL;
    }
}

// Send Data method
// returns TRUE if all the data was sent:
- (BOOL)sendData:(void*)buffer length:(UInt32)length
{
    if ( mRFCOMMChannelRef != nil )
    {
        UInt32				numBytesRemaining;
        IOReturn			result;
		BluetoothRFCOMMMTU	rfcommChannelMTU;
		
		result = kIOReturnSuccess;
		numBytesRemaining = length;
		
		// Get the RFCOMM Channel's MTU.  Each write can only contain up to the MTU size
		// number of bytes.
		rfcommChannelMTU = IOBluetoothRFCOMMChannelGetMTU( mRFCOMMChannelRef );
		
		while ( ( result == kIOReturnSuccess ) && ( numBytesRemaining > 0 ) )
		{
			// finds how many bytes I can send:
			UInt32 numBytesToSend = ( ( numBytesRemaining > rfcommChannelMTU ) ? rfcommChannelMTU :  numBytesRemaining );
			
			// This function won't return until the buffer has been passed to the Bluetooth hardware
			// to be sent to the remote device.
			// Alternatively, the asynchronous version of this function could be used which would queue
			// up the buffer and return immediately.
			result = IOBluetoothRFCOMMChannelWriteSync( mRFCOMMChannelRef, buffer, numBytesToSend );
			
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
// returns nil if not connected:
- (NSString *)remoteDeviceName
{
    return (NSString *)IOBluetoothDeviceGetName( IOBluetoothRFCOMMChannelGetDevice( mRFCOMMChannelRef ) );
}

// Callback for RFCOMM Events:
void rfcommEventListener( IOBluetoothRFCOMMChannelRef rfcommChannelRef, void *refCon, IOBluetoothRFCOMMChannelEvent *event )
{
    NSLog(@"Got new RFCOMM Event %p %d\n", rfcommChannelRef, event->eventType);
    [(ChatBluetoothInterface_C *)refCon handleIncomingEvent:event];
}

// Event Handler:
// handles events from the RFCOMM channel:
- (void)handleIncomingEvent:(IOBluetoothRFCOMMChannelEvent *)event
{
    switch ( event->eventType )
    {
            case kIOBluetoothRFCOMMNewDataEvent:
                [mNewDataTarget performSelector:mHandleNewDataSelector withObject:[NSData dataWithBytes:event->u.newData.dataPtr length:event->u.newData.dataSize]];
            break;
            
            case kIOBluetoothRFCOMMChannelTerminatedEvent:
                [mRemoteDisconnectionTarget performSelector:mHandleRemoteDisconnectionSelector];
            break;
			
			default:
			// For the moment ignore the other events
			break;
    }
}

@end

#endif // USE_C_API
