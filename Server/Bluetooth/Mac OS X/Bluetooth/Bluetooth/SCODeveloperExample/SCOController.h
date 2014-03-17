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

/*
	This little example will show you how to make your own
	SCO audio agent to support extended device features for Handsfree and Headset devices.
	On Leopard release we will only support the bare-minimum to make these devices work
	properly, anything extra is up to you.  

	We've made the device bring-up really simple as you'll see in the source counterpart
	of this file.  Included is a simple subclass of the IOBluetoothHandsFreeGateway 
	object, you'll be handling the feature data in there.

	You'll notice that there is a parser in main.m to support blued's launch args so 
	this app can be launch from incoming connections. There is however some work you'll need to 
	do to get it to work. To make this agent launch on incoming connections you'll need to either 
	change the "target application" setting in the Headset SDP record (plist) located in the 
	IOBluetooth framework Resources, or load up one of your own.  If you choose to change that 
	record you'll probably need to tickle the SDP system / blued to make sure it doesn't use the 
	cached version of the record.

	And thats pretty much it.  Set some features, add some handlers, change the SDP, and enjoy! 
 */
 

#import <Cocoa/Cocoa.h>

@class MyHandsFreeGateway;
@class IOBluetoothHeadsetDevice;

@interface SCOController : NSObject {
	
	MyHandsFreeGateway *			mHandsFreeGateway;
	IOBluetoothHeadsetDevice *		mHeadsetDevice;

	BOOL							mIsDoneLaunching;

	NSTimer *						mPlayOrDieTimer;
	NSTimer *						mFinishConnectionTimer;
	NSTimer *						mServiceLevelTimer;
}

- (void) prepareSCOAudioServices;

- (void) shutdown;
- (void) shutdownWithError:(IOReturn)inError;

- (IOReturn) setPlayOrDieTimer;
- (IOReturn) resetPlayOrDieTimer;

- (IOReturn) setFinishConnectionTimer;
- (IOReturn) resetFinishConnectionTimer;


@end
