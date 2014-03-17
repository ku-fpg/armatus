//  RFCOMM_Open_SPP_Example
//
//  Created by Marco Pontil on 12/18/04.
//  Copyright Apple Computer, Inc. 2004. All rights reserved.
//

/* RFCOMMSPPTestController */

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

#import <Cocoa/Cocoa.h>

// Bluetooth headers:
#import <IOBluetooth/objc/IOBluetoothDevice.h>
#import <IOBluetooth/objc/IOBluetoothSDPUUID.h>
#import <IOBluetooth/objc/IOBluetoothRFCOMMChannel.h>
#import <IOBluetoothUI/objc/IOBluetoothDeviceSelectorController.h>


@interface RFCOMMSPPTestController : NSObject
{
    IBOutlet id mCloseButton;
    IBOutlet id mLogASCIIView;
    IBOutlet id mLogHEXView;
    IBOutlet id mOpenButton;
    IBOutlet id mRFCOMMSPPTestWindow;
	
	// Bluetooth variables:
	IOBluetoothDevice *mBluetoothDevice;
	IOBluetoothRFCOMMChannel *mRFCOMMChannel;
}

// Methods to interact with the window:

- (IBAction)closeConnectonAction:(id)sender;
- (IBAction)openConnectionAction:(id)sender;

- (void)addThisByteToTheLogs:(unsigned char)byte;
- (void)logString:(NSString *)string onView:(NSTextView *)view;

// Delegate on window to know when the user closes the window
// so we can close everything.
- (BOOL)windowShouldClose:(id)sender;

// Methods to handle the Baseband and RFCOMM connection:
- (BOOL)openSerialPortProfile;
- (void)closeRFCOMMConnectionOnChannel:(IOBluetoothRFCOMMChannel*)channel;
- (void)closeDeviceConnectionOnDevice:(IOBluetoothDevice*)device;

// These are methods that are called when "things" happen on the
// bluetooth connection, read along and it will all be clearer:

// Called by the RFCOMM channel on us once the baseband and rfcomm connection is completed:
- (void)rfcommChannelOpenComplete:(IOBluetoothRFCOMMChannel*)rfcommChannel status:(IOReturn)error;

// Called by the RFCOMM channel on us when new data is received from the channel:
- (void)rfcommChannelData:(IOBluetoothRFCOMMChannel *)rfcommChannel data:(void *)dataPointer length:(size_t)dataLength;

// Called by the RFCOMM channel on us when something happens and the connection is lost:
- (void)rfcommChannelClosed:(IOBluetoothRFCOMMChannel *)rfcommChannel;
 
@end
