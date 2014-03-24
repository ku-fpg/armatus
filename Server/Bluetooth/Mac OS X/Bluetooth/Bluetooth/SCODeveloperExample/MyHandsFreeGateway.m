
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

#import <IOBluetooth/objc/IOBluetoothRFCOMMChannel.h>

#define Log( FMT, ARGS...)		NSLog( @ FMT, ## ARGS)

#import "MyHandsFreeGateway.h"

@implementation MyHandsFreeGateway


- (void) rfcommChannelData:(IOBluetoothRFCOMMChannel *)rfcommChannel data:(void *)data length:(size_t)length
{
	char *	atCommand;
	UInt8 response[12];
	char * d;
	UInt32 len;
	
	d = (char *)data;
	
	Log("[data] entry (len == %d)\n", length);
	
	// At least 3 characters (i.e. AT plus something else)
	if( length <= 2 ) 
	{
		Log("[data] ERR: length is too short\n");
		goto exit;
	}
	// AT command
	if( strncmp( d, "AT", 2 ) != 0 )
	{
		Log("[data] ERR: First two bytes are no good: \'%c%c\'\n", d[0], d[1]);
		goto exit;
	}
	// look for the terminator		
	if( d[length-1] != '\r' )
	{
		Log("[data] ERR: data is not terminated: 0x%x\'\n",  d[length-1] );
		goto exit;
	}
	
	// Change \r to \0 so we can use generic strcmp() 
	d[length-1] = '\0';
	atCommand = &(d[2]);
	
	//  BRSF
	//
	if ( strncmp( atCommand, "+BRSF=", 6 ) == 0 )		// AT+BRSF=
	{		
		Log("[data]  AT%s \n", atCommand);
		
		if ( length >= 10 )
		{
			[self setGatewaySupportedFeatures: atoi(&atCommand[6]) ];
		}
		
		len = sprintf( (char*)response, "\r\n+BRSF:%ld\r\n", mGatewaySupportedFeatures);
		Log("[data]    response: '%s'  len: %d\n", (char*)response, len);
		[self sendRFCOMMData:(const void *)response length: len];
		
		len = sprintf( (char*)response, "\r\nOK\r\n");
		Log("[data]    sending 'OK'\n");
		[self sendRFCOMMData:(const void *)response length: len];
	}
	
	else
	{
		Log("[data] Unknown Command: 'AT%s', maybe super knows?\n");
		
		[super rfcommChannelData:rfcommChannel data:data length:length];
	}
	
exit:
		
	return;
}




@end
