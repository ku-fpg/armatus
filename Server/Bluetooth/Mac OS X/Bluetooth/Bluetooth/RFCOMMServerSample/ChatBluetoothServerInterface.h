/*
	File: ChatBluetoothServerInterface.h
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

/* ChatBluetoothServerInterface */

#import <Foundation/Foundation.h>

// Bluetooth Objective C Headers:
#import <IOBluetooth/objc/IOBluetoothSDPServiceRecord.h>
#import <IOBluetooth/objc/IOBluetoothRFCOMMChannel.h>

// Bluetooth C Headers:
#include <IOBluetooth/IOBluetoothUserLib.h>

@class IOBluetoothUserNotification;

@interface ChatBluetoothServerInterface : NSObject {
    // This is the RFCOMM channel we connect to:
    IOBluetoothRFCOMMChannel		*mRFCOMMChannel;

    // Service Entry for the service we publish
    BluetoothRFCOMMChannelID		mServerChannelID;
    BluetoothSDPServiceRecordHandle	mServerHandle;
    
    IOBluetoothUserNotification		*mIncomingChannelNotification;
    
    // This is the method to call in the UI when new data shows up:
    SEL	mHandleNewDataSelector;
    id	mNewDataTarget;
    
    // This is the method to call when the RFCOMM channel disappears.
    SEL	mHandleRemoteDisconnectionSelector;
    id	mDisconnectionTarget;
    
    // This is the method to call when the RFCOMM channel disappears.
    SEL mHandleRemoteConnectionSelector;
    id	mConnectionTarget;
}

// Connection Method:
// returns TRUE if the connection was successful:
- (BOOL)publishService;

// Stops Providing chat services
// Removes the published services from the SDP dictionary:
// If a connection is in progress it will not interrupt it.
- (void)stopProvidingChatServices;

// Disconnection:
// closes the channel:
- (void)disconnectFromClient;

// New RFCOMM channel show up:
// A new RFCOMM channel shows up we have to make sure it is the one we are waiting for:
-(void) newRFCOMMChannelOpened:(IOBluetoothUserNotification *)inNotification channel:(IOBluetoothRFCOMMChannel *)newChannel;

// Returns the name of the device we are connected to
// returns nil if not connection:
- (NSString *)remoteDeviceName;

// Returns the name of the local bluetooth device
- (NSString *)localDeviceName;

// Send Data method
// returns TRUE if all the data was sent:
- (BOOL)sendData:(void *)buffer length:(UInt32)length;

// IOBluetoothRFCOMMChannel delegate methods
- (void)rfcommChannelData:(IOBluetoothRFCOMMChannel*)rfcommChannel data:(void *)dataPointer length:(size_t)dataLength;
- (void)rfcommChannelClosed:(IOBluetoothRFCOMMChannel*)rfcommChannel;

// Registers selector for incoming data:
// tells to this class to call myTarget and myTargetAction when new data shows up:
- (void)registerForNewData:(id)myTarget action:(SEL)actionMethod;

// Registers selector for disconnection:
// tells to this class to call myTarget and myTargetAction when the channel disconnects:
- (void)registerForTermination:(id)myTarget action:(SEL)actionMethod;

// Registers selector for a successful completed connection:
// tells to this class to call myTarget and myTargetAction when the channel disconnects:
- (void)registerForNewConnection:(id)myTarget action:(SEL)actionMethod;


@end
