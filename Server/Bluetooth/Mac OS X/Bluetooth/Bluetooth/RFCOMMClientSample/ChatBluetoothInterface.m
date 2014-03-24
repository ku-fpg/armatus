/*
	File: ChatBluetoothInterface.m
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

#import "ChatBluetoothInterface.h"

#import <IOBluetooth/IOBluetoothUserLib.h>

// This is a custom UUID we have defined for our example chat service

unsigned char gExampleChatServiceClassUUID[] =	// 0DAD4365-5DF1-11D6-9F6E-00039353E858
    {
        0x0d, 0xad, 0x43, 0x65, 0x5d, 0xf1, 0x11, 0xd6,
        0x9f, 0x6e, 0x00, 0x03, 0x93, 0x53, 0xe8, 0x58
    };
    
// See the end of this file for information on how to generate new UUIDs

@implementation ChatBluetoothInterface

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
	mRemoteDisconnectionTarget = myTarget;
}

// Returns the name of the local bluetooth device
- (NSString *)localDeviceName
{
    BluetoothDeviceName localDeviceName;

    if ( IOBluetoothLocalDeviceReadName( localDeviceName, NULL, NULL, NULL ) == kIOReturnSuccess )
    {
        return [NSString stringWithUTF8String:(const char*)localDeviceName];
    }

    return nil;
}

// Abstract methods that the subclass needs to implement:

- (BOOL)sendData:(void*)buffer length:(UInt32)length
{
	// Subclass must implement
	return NO;
}

- (NSString *)remoteDeviceName
{
	// Subclass must implement
	return nil;
}

- (BOOL)connectToServer
{
	// Subclass must implement
	return NO;
}

- (void)disconnectFromServer
{
	// Subclass must implement
	return;
}

@end

/*
Sample code to generate UUID:
=============================

#include <CoreFoundation/CoreFoundation.h>

int main()
{
        CFUUIDRef       uuid;
        CFStringRef     string;

        uuid = CFUUIDCreate( NULL );
        string = CFUUIDCreateString( NULL, uuid );

        CFShow( string );
}

*/
