/*
	File: ChatClientController.m
	Constains: UI for Bluetooth sample [not to be used as UI sample code]
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

#import "ChatClientController.h"

#ifdef USE_C_API
#import "ChatBluetoothInterface_C.h"
#else
#import "ChatBluetoothInterface_ObjC.h"
#endif

// NOTE: This is NOT an UI example, the user interface in this application is purposely
// minimal. Do not consider using this file as code sample for your applications.

@implementation ChatClientController

// Object Allocation and Deallocation
- init
{
    self = [super init];

#ifdef USE_C_API
    myBluetoothInterface = [[ChatBluetoothInterface_C alloc] init];
#else
    myBluetoothInterface = [[ChatBluetoothInterface_ObjC alloc] init];
#endif
    
    // Registes the callbacks for the data and for disconnection:
    [myBluetoothInterface registerForTermination:self action:@selector(chatHandleRemoteDisconnection)];

    // gets the local device name:
    localDeviceName = [myBluetoothInterface localDeviceName];
    [localDeviceName retain];
    
    return self;
}

- (void) dealloc
{
    [localDeviceName release];
    [myBluetoothInterface release];
    [super dealloc];
}

- (BOOL) windowShouldClose: (NSWindow*) sender
{
    [myBluetoothInterface disconnectFromServer];
    
    exit(0);
    
    return TRUE;
}

// UI Handlers.
- (IBAction)chatActionOnConnect:(id)sender
{
    if ([myBluetoothInterface connectToServer] == FALSE)
    {
        NSBeep();
        
        [self chatHandleRemoteDisconnection];
    }
    else
    {
        [chatDisconnectButton setEnabled:TRUE];
        [chatInputTextField setEnabled:TRUE];
        [myBluetoothInterface registerForNewData:self action:@selector(chatHandleNewData:)];
    }
}

- (IBAction)chatActionOnDisconnect:(id)sender
{
    [myBluetoothInterface disconnectFromServer];
    [self chatHandleRemoteDisconnection];
}

- (IBAction)chatActionOnMessageTextField:(id)sender
{
    NSRange      theRange;
    unsigned int start;
    NSMutableString    *theString;

    // Send the message trough the bluetooth channel:
    theString = [NSMutableString stringWithFormat:@"%@\n",[chatInputTextField stringValue]];
    
    [myBluetoothInterface sendData:(void*)[theString UTF8String] length:[theString length]];

    // Send the message trough the bluetooth channel:
    theString = [NSMutableString stringWithFormat:@"%@: %@",localDeviceName , theString];

    // Dump it on the screen
    start = [[chatOutputTextField string] length];
    theRange = NSMakeRange(start, 0 );
    
    [chatOutputTextField replaceCharactersInRange:theRange withString:theString];
    theRange = NSMakeRange(start, [theString length] );
    [chatOutputTextField setTextColor:[NSColor blueColor] range:theRange];
    
    // Clears the input text field:
    [chatInputTextField setStringValue:@""];
}


// Bluetooth Handlers
- (void)chatHandleRemoteDisconnection
{
    [chatDisconnectButton setEnabled:FALSE];
    [chatInputTextField setEnabled:FALSE];
    
    [myBluetoothInterface registerForNewData:nil action:nil];
}

- (void)chatHandleNewData:(NSData*)dataObject
{
    NSRange      theRange;
    unsigned int start;
    NSString    *theString;
    
    // Dump the message on the screen
    start = [[chatOutputTextField string] length];
    theRange = NSMakeRange(start, 0 );
    
    theString = [NSMutableString stringWithFormat:@"%@: %@",[myBluetoothInterface remoteDeviceName] , [[[NSString alloc] initWithBytes:[dataObject bytes] length:[dataObject length]] autorelease]];
    
    [chatOutputTextField replaceCharactersInRange:theRange withString:theString];
    theRange = NSMakeRange(start, [theString length] );
    [chatOutputTextField setTextColor:[NSColor redColor] range:theRange];
}

@end
