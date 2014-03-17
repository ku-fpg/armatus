/*
	File:		MyWindow.m
	Contains:	Sample code for using the OBEXSession APIs available in the IOBluetooth Framework. This
				example uses the objective-C API for the session. See other examples for usage of the C API.
				Note: this code probably does not represent the best way to write a full-blown application
				using the Bluetooth/OBEX APIs. it is merely meant as a primer on how to get the ball rolling. 
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


#import "MyWindow.h"

#include <IOBluetooth/IOBluetoothUtilities.h>
#include <IOBluetooth/OBEXBluetooth.h>

#import <IOBluetoothUI/objc/IOBluetoothServiceBrowserController.h>
#import <IOBluetooth/objc/IOBluetoothSDPServiceRecord.h>

#define	DEBUG_NAME	"[OBEXSample]"


@implementation MyWindow


#pragma mark -
#pragma mark === IB Actions ===

//=================================================================================================
//	buttonOBEXConnectClicked
//=================================================================================================

- (IBAction)buttonOBEXConnectClicked:(id)sender
{
	OBEXError					status;
	BluetoothDeviceAddress		deviceAddress;
	BluetoothRFCOMMChannelID	channelID;
		
	if( !IOBluetoothLocalDeviceAvailable() )
	{	
		[self updateErrorText:@"No device available.\n" ];
		return;
	}
	
 	if( !mOBEXSession )
	{
		channelID = [TextFieldChannelID intValue];
		mMaxPacketLength = [TextFieldPacketSize intValue];
		
		if( mMaxPacketLength > 0x2000 )
		{
			mMaxPacketLength = 0x2000;
		}
		
		// Create session with browser or manually.
		
		status = IOBluetoothNSStringToDeviceAddress( [TextFieldAddress stringValue], &deviceAddress );
		if( status == kOBEXSuccess )
		{
		
			[self updateErrorText:@"Opening connection...\n"];
	
			// Create session ref, using service we got from the browser.
					
			mOBEXSession = [IOBluetoothOBEXSession	withDevice:[IOBluetoothDevice withAddress:&deviceAddress]
													channelID:channelID];
			if( !mOBEXSession )
			{
				[self updateErrorText:[NSString stringWithFormat:@"An error occurred(1.5): 0x%x\n", status ]];
				return;
			}
			
			[mOBEXSession retain];
		}
		else
		{
			IOBluetoothSDPServiceRecord * record;
			
			// Get browser to get a service ref.
	
			status = [IOBluetoothServiceBrowserController	browseDevicesAsSheetForWindow:&record
															options:kIOBluetoothServiceBrowserControllerOptionsNone
															window:self];
			if( status == kIOBluetoothUIUserCanceledErr )
			{
				[self updateErrorText:@"Canceled by user.\n"];
				return;
			}
			else if( status != kOBEXSuccess )
			{
				[self updateErrorText:[NSString stringWithFormat:@"An error occurred(2): 0x%x\n", status ]];
				return;
			}

			mOBEXSession = [IOBluetoothOBEXSession withSDPServiceRecord:record];
			if( !mOBEXSession )
			{
				[self updateErrorText:[NSString stringWithFormat:@"An error occurred(3): 0x%x\n", status ]];
				return;
			}

			[mOBEXSession retain];
		}

		if( [mOBEXSession hasOpenOBEXConnection] == FALSE )
		{
			// Make an OBEX connection.
			status = [mOBEXSession	OBEXConnect:(OBEXFlags)kOBEXConnectFlagNone
									maxPacketLength:(OBEXMaxPacketLength)mMaxPacketLength
									optionalHeaders:(void*)NULL
									optionalHeadersLength:(size_t)0
									eventSelector:@selector( commandSentCallback: )
									selectorTarget:(void *)self
									refCon:NULL];
			if( status != kOBEXSuccess )
			{
				[self updateErrorText:[NSString stringWithFormat:@"OBEXConnect Error occurred: 0x%x\n", status ]];
				[mOBEXSession release];
				mOBEXSession = NULL;
				[self resetButtons];
			}
		}
	}
	else
	{
		if( [mOBEXSession hasOpenOBEXConnection] )
		{
			// Disconnect OBEX Session.
		
			status = [mOBEXSession	OBEXDisconnect:NULL
									optionalHeadersLength:0
									eventSelector:@selector( commandSentCallback: )
									selectorTarget:(void *)self
									refCon:NULL];
			if( status != kOBEXSuccess )
			{
				[self updateErrorText:[NSString stringWithFormat:@"OBEXDisconnect failed. 0x%x\n", status ]];
			}
		}
		else
		{
			// Make an OBEX connection.
			
			status = [mOBEXSession	OBEXConnect:(OBEXFlags)kOBEXConnectFlagNone
									maxPacketLength:(OBEXMaxPacketLength)mMaxPacketLength
									optionalHeaders:(void*)NULL
									optionalHeadersLength:(size_t)0
									eventSelector:@selector( commandSentCallback: )
									selectorTarget:(void *)self
									refCon:NULL];
			if( status != kOBEXSuccess )
			{
				[self updateErrorText:[NSString stringWithFormat:@"OBEXConnect Error occurred: 0x%x\n", status ]];
				[mOBEXSession release];
				mOBEXSession = NULL;
				[self resetButtons];
			}
		}
	}
}

//=================================================================================================
//	buttonDeviceDisconnectClicked
//=================================================================================================

- (IBAction)buttonDeviceDisconnectClicked:(id)sender
{
	if( mOBEXSession )
	{
		[[mOBEXSession getDevice] closeConnection]; 

		[mOBEXSession release];
		mOBEXSession = NULL;
	
		[ButtonOBEXConnectDisconnect setTitle:@"Connect"];
	}
	
	[self resetButtons];
}

//=================================================================================================
//	buttonSendGetClicked
//=================================================================================================

- (IBAction)buttonSendGetClicked:(id)sender
{
	OBEXError				status;
	int						i;
	CFMutableDictionaryRef	dictionary;
	
	dictionary = CFDictionaryCreateMutable(	kCFAllocatorDefault,
											0,
											&kCFTypeDictionaryKeyCallBacks,
											&kCFTypeDictionaryValueCallBacks );
	
	// Package up desired headers.

	if( [[TextFieldFileToGet stringValue] length] > 0 )
	{
		OBEXAddNameHeader( (CFStringRef) [TextFieldFileToGet stringValue], dictionary );
	}
	else
	{
		OBEXAddTypeHeader( CFSTR( "text/x-vCard" ), dictionary ); 
	}

	mGetHeadersDataRef = OBEXHeadersToBytes( dictionary );
	
	// Show our header in th debug textfield.
	{
		[self insertText:[NSString stringWithCString:(const char *)CFDataGetBytePtr( mGetHeadersDataRef ) length:CFDataGetLength( mGetHeadersDataRef )] withColor:[NSColor blueColor]];
		[self insertText:[NSString stringWithCString:"\n" length:1] withColor:[NSColor blueColor]];

		// print it out.
	
		printf( DEBUG_NAME "[buttonSendGetClicked] data to send (dataLength = 0x%02x):\n      ", (int) CFDataGetLength( mGetHeadersDataRef ) );
		for( i = 0; i < CFDataGetLength( mGetHeadersDataRef ); i++ )
		{
			printf( "%02x ", CFDataGetBytePtr( mGetHeadersDataRef )[i] );
		}
		printf( "\n" );
	}
	
	// Send the command.
	
	status = [mOBEXSession	OBEXGet:TRUE
							headers:(void*) CFDataGetBytePtr( mGetHeadersDataRef )
							headersLength:CFDataGetLength( mGetHeadersDataRef )
							eventSelector:@selector( commandSentCallback: )
							selectorTarget:(void *)self
							refCon:self];
	if( status != kOBEXSuccess )
	{
		CFRelease( mGetHeadersDataRef );
		mGetHeadersDataRef = NULL;
	
		[self resetButtons];
		[self updateErrorText:[NSString stringWithFormat:@"OBEXSessionGet failed. Error = 0x%x.\n", status ]];
	}
}


//=================================================================================================
//	buttonSendPutClicked
//=================================================================================================

- (IBAction)buttonSendPutClicked:(id)sender
{
	OBEXError				status;
	CFMutableDictionaryRef	dictionary;
	Boolean					isLastChunk = FALSE;
	
	mCurrentFileSize = 0;
	mLastFileOffset = 0;
	[PutProgressBar setDoubleValue:0.0];
	
	// open the file, get it's data.
	
	mCurrentFile = [NSFileHandle fileHandleForReadingAtPath:[self getPathOfFileAsPeerToApp:[TextFieldFileToPut stringValue]]];
	if( mCurrentFile )
	{
		mCurrentFileSize = [mCurrentFile seekToEndOfFile];
		[mCurrentFile seekToFileOffset:0];
		if( mCurrentFileSize == 0 )
		{
			[self updateErrorText:[NSString stringWithFormat:@"File's length is zero!\n"]];
			mCurrentFile = NULL;
			goto exit;
		}
		[mCurrentFile retain];
	}
	else
	{
		[self updateErrorText:[NSString stringWithFormat:@"Bad filename, could not open file to put. Does it have a hidden extension? Is it located next to the application?\n"]];
		goto exit;
	}
	
	// Package up desired headers.

	dictionary = CFDictionaryCreateMutable(	kCFAllocatorDefault,
											0,
											&kCFTypeDictionaryKeyCallBacks,
											&kCFTypeDictionaryValueCallBacks );

	OBEXAddNameHeader( (CFStringRef) [TextFieldFileToPutNewName stringValue], dictionary );

	mPutHeadersDataRef = OBEXHeadersToBytes( dictionary );
		
	// Display what we are going to send in the debugging textedit.
	
	{		
		[self insertText:[NSString stringWithCString:(const char *)CFDataGetBytePtr( mPutHeadersDataRef ) length:CFDataGetLength( mPutHeadersDataRef )] withColor:[NSColor blueColor]];
		[self insertText:[NSString stringWithCString:"\n" length:1] withColor:[NSColor blueColor]];
	
		[self insertText:@"Starting PUT...\n" withColor:[NSColor blueColor]];
	}
	
	// Get the first chunk of the file for our put. The getNextFileChunk will make sure we only get what will
	// fit in a single packet, and we'll send more when we get a successful put command response.

	mTempPutDataBuffer = [self	getNextFileChunk:(NSFileHandle*)mCurrentFile
								optionalHeaderLength:CFDataGetLength( mPutHeadersDataRef )
								isLastChunk:&isLastChunk];

	status = [mOBEXSession	OBEXPut:isLastChunk
							headersData:(void*) CFDataGetBytePtr( mPutHeadersDataRef )
							headersDataLength:CFDataGetLength( mPutHeadersDataRef )
							bodyData:(void*) [mTempPutDataBuffer bytes]
							bodyDataLength:[mTempPutDataBuffer length]
							eventSelector:@selector( commandSentCallback: )
							selectorTarget:(void *)self
							refCon:NULL];
	if( status != kOBEXSuccess )
	{
		[self resetButtons];
		[self updateErrorText:[NSString stringWithFormat:@"OBEXSessionPut failed. Error = 0x%x.\n", status ]];
	}

exit:
	return;
}


//=================================================================================================
//	buttonAbortCommandClicked
//=================================================================================================

- (IBAction)buttonAbortCommandClicked:(id)sender
{
	// Save request for later, we might be in the middle of a transaction hat would screw
	// up our state or confuse the partner we are OBEXing with.
	
	mUserWantsAbort = TRUE;
}

//=================================================================================================
//	buttonSendSetPathClicked
//=================================================================================================

- (IBAction)buttonSendSetPathClicked:(id)sender
{
	OBEXError				status;
	int						i;
	CFMutableDictionaryRef	dictionary;	
		
	// Get pathName to request from remote device.

	NSLog( @"pathname used: %@\n", [TextFieldSetPath stringValue] );

	// Package up desired headers.

	dictionary = CFDictionaryCreateMutable(	kCFAllocatorDefault,
											0,
											&kCFTypeDictionaryKeyCallBacks,
											&kCFTypeDictionaryValueCallBacks );

	OBEXAddNameHeader( (CFStringRef) [TextFieldSetPath stringValue], dictionary );

	mSetPathHeadersDataRef = OBEXHeadersToBytes( dictionary );
		
	// Display what we are going to send in the debugging textedit.
	
	{		
		[self insertText:[NSString stringWithCString:(const char *)CFDataGetBytePtr( mSetPathHeadersDataRef ) length:CFDataGetLength( mSetPathHeadersDataRef )] withColor:[NSColor blueColor]];
		[self insertText:[NSString stringWithCString:"\n" length:1] withColor:[NSColor blueColor]];

		printf( DEBUG_NAME "[ButtonSendSetPathClicked] data to send (dataLength = 0x%02x):\n      ", (int) CFDataGetLength( mSetPathHeadersDataRef ) );
		for( i = 0; i < CFDataGetLength( mSetPathHeadersDataRef ); i++ )
		{
			printf( "%02x ", CFDataGetBytePtr( mSetPathHeadersDataRef )[i] );
		}
		printf( "\n" );
	}
	
	// Send the command. Note that we are not careful here at all about the header dictionary--
	// theoretically, it could go across multiple Puts if it is larger than the max packet size,
	// so you should really keep it around until your SetPathCallback gets called with a success message.
	// At that point you can release the header CFDataRef.

	status = [mOBEXSession	OBEXSetPath:0x02
							constants:0
							optionalHeaders:(void*)CFDataGetBytePtr( mSetPathHeadersDataRef )
							optionalHeadersLength:CFDataGetLength( mSetPathHeadersDataRef )
							eventSelector:@selector( commandSentCallback: )
							selectorTarget:(void *)self
							refCon:NULL];
	if( status != kOBEXSuccess )
	{
		[self resetButtons];
		[self updateErrorText:[NSString stringWithFormat:@"OBEXSessionSetPath failed. Error = 0x%x.\n", status ]];
	}
}

#pragma mark -
#pragma mark === Support stuff ===

//=================================================================================================
//	performAbort
//=================================================================================================

-(OBEXError)performAbort
{
	OBEXError	status = kOBEXSuccess;
	
	mAbortSent = TRUE;
	
	if( mOBEXSession && [mOBEXSession hasOpenTransportConnection] )
	{
		status = [mOBEXSession OBEXAbort:(void*)NULL
								optionalHeadersLength:(size_t)0
								eventSelector:@selector( commandSentCallback: )
								selectorTarget:(void *)self
								refCon:NULL];
		if( status != kOBEXSuccess )
		{
			[self updateErrorText:[NSString stringWithFormat:@"OBEXSessionAbort failed. Error = 0x%x.\n", status ]];
		}
	}
	
	return( status );
}

//=================================================================================================
//	insertText
//=================================================================================================

- (void) insertText:(NSString *)string withColor:(NSColor *)color
{
    NSRange			theRange;
    NSRange			selRange;
    unsigned int	start;
    Boolean			scrollToVis = FALSE;

    // Scroll to new text if the scroller is at the bottom and the selection is 0 length
    // and the insertion point is at the end.
    selRange = [TextViewData selectedRange];
    if( ([[TextViewScroller verticalScroller] floatValue] == 1.0) && (selRange.length == 0) )
	{
        scrollToVis = TRUE;
	}
        
    start = [[TextViewData	string] length];
    theRange = NSMakeRange(start, 0 );
    [TextViewData replaceCharactersInRange:theRange withString:string];
    theRange = NSMakeRange(start, [string length] );
    [TextViewData setTextColor:color range:theRange];
    if(scrollToVis)
    {
        theRange = [TextViewData selectedRange];
        [TextViewData scrollRangeToVisible:theRange];
    }
    return;
} 

//=================================================================================================
//	resetButtons
//=================================================================================================

- (void)resetButtons
{
	[ButtonAbortCommand setEnabled:FALSE];
	[ButtonOBEXConnectDisconnect setEnabled:TRUE];
	[ButtonDeviceDisconnect setEnabled:FALSE];
	[ButtonSendGet setEnabled:FALSE];
	[ButtonSendPut setEnabled:FALSE];
	[ButtonSendSetPath setEnabled:FALSE];
}

//====================================================================================================================
//	getNextFileChunk
//====================================================================================================================

-(NSData*)getNextFileChunk:(NSFileHandle*)file
			optionalHeaderLength:(size_t)optHdrLength
			isLastChunk:(Boolean*)outIsLastChunk
{
	OBEXMaxPacketLength	roomForBodyData;
	
	roomForBodyData = [mOBEXSession getAvailableCommandPayloadLength:kOBEXOpCodePut];
	if( roomForBodyData == 0 ) return( nil );

	if( roomForBodyData <= optHdrLength )
	{
		// We don't even have enough room for optional headers. This is a problem!
		
		return( nil );
	}

	// Subtract out how much data we have in our optional headers, because we need to send those too
	// and they could take up some space.
	
	roomForBodyData -= optHdrLength;

	if( mTempPutDataBuffer )
	{
		[mTempPutDataBuffer release];
		mTempPutDataBuffer = NULL;
	}
	
	if( mLastFileOffset + roomForBodyData >= mCurrentFileSize )
	{
		// this will be the last chunk.
		if( outIsLastChunk )
		{
			*outIsLastChunk = TRUE;
		}
	}
	else
	{
		if( outIsLastChunk ) *outIsLastChunk = FALSE;
	}

	[file seekToFileOffset:mLastFileOffset];
	
	mTempPutDataBuffer =  [file readDataOfLength:roomForBodyData];
	if( mTempPutDataBuffer )
	{
		mLastFileOffset += [mTempPutDataBuffer length];
		[mTempPutDataBuffer retain];
	}
	
	return( mTempPutDataBuffer );
}


//=================================================================================================
//	getPathOfFileAsPeerToApp
//=================================================================================================

- (NSString*)getPathOfFileAsPeerToApp:(NSString*)inFileName
{
	NSString * outString;
	
	outString = [[NSBundle mainBundle] bundlePath];
	if( outString )
	{
		outString = [outString stringByDeletingLastPathComponent];
		outString = [outString stringByAppendingString:@"/"];
		outString = [outString stringByAppendingString:inFileName];
	}
	
	return( outString );
}

//=================================================================================================
//	updateErrorText
//=================================================================================================

- (void)updateErrorText:(NSString*)inString
{
	[self insertText:inString withColor:[NSColor redColor]];

}

#pragma mark -
#pragma mark === OBEX Client Session Selectors ===


//====================================================================================================================
//	commandSentCallback
//====================================================================================================================

- (void)commandSentCallback:(const OBEXSessionEvent *)inSessionEvent
{
	NSLog( @"[ObjectExchangeReceiver][commandSentCallback] entered. inSessionEvent->type = %x", inSessionEvent->type );

	// If the user wants ABORT, send an abort. We SHOULD be more careful here, because it could be
	// that this message is at the end of a received transaction, and in which case that doesn't
	// make any sense to send an Abort, since we are done. But, we're not going to set a good example
	// in this case.
	
	if( mUserWantsAbort )
	{
		[self performAbort];
		mUserWantsAbort = FALSE;
		return;
	}
	
	switch( inSessionEvent->type )
	{
		case( kOBEXSessionEventTypeConnectCommandResponseReceived ):
		{
			mMaxPacketLength = inSessionEvent->u.connectCommandResponseData.maxPacketSize;
		
			NSLog( @"[ObjectExchangeReceiver][commandSentCallback] kOBEXSessionEventTypeConnectCommandResponseReceived" );
			[self OBEXConnectHandler:&inSessionEvent->u.connectCommandResponseData];
			break;
		}
		case( kOBEXSessionEventTypeDisconnectCommandResponseReceived ):
		{
			NSLog( @"[ObjectExchangeReceiver][commandSentCallback] kOBEXSessionEventTypeDisconnectCommandResponseReceived" );
			[self OBEXDisconnectHandler:&inSessionEvent->u.disconnectCommandResponseData];
			break;
		}
		case( kOBEXSessionEventTypePutCommandResponseReceived ):
		{
			NSLog( @"[ObjectExchangeReceiver][commandSentCallback] kOBEXSessionEventTypePutCommandResponseReceived" );
			[self OBEXPutHandler:&inSessionEvent->u.putCommandResponseData];
			break;
		}
		case( kOBEXSessionEventTypeGetCommandResponseReceived ):
		{
			NSLog( @"[ObjectExchangeReceiver][commandSentCallback] kOBEXSessionEventTypeGetCommandResponseReceived" );
			[self OBEXGetHandler:&inSessionEvent->u.getCommandResponseData];
			break;
		}
		case( kOBEXSessionEventTypeSetPathCommandResponseReceived ):
		{
			NSLog( @"[ObjectExchangeReceiver][commandSentCallback] kOBEXSessionEventTypeSetPathCommandResponseReceived" );
			[self OBEXSetPathHandler:&inSessionEvent->u.setPathCommandResponseData];
			break;
		}
		case( kOBEXSessionEventTypeAbortCommandResponseReceived ):
		{
			NSLog( @"[ObjectExchangeReceiver][commandSentCallback] kOBEXSessionEventTypeAbortCommandResponseReceived" );
			[self OBEXAbortHandler:&inSessionEvent->u.abortCommandResponseData];
			break;
		}
		default:
		{
			/* Don't recognize command type (could be something new!), so ignore it */
			NSLog( @"[ObjectExchangeReceiver][commandSentCallback] unrecognized command type." );
			break;
		}
	}
	
		
	NSLog( @"[ObjectExchangeReceiver][commandSentCallback] exit." );
	return;
}

//====================================================================================================================
//	OBEXConnectHandler
//====================================================================================================================

- (void)OBEXConnectHandler:(const OBEXConnectCommandResponseData*)inOBEXSessionEventData
{
	printf( DEBUG_NAME "[OBEXConnectHandler] Entry. inOBEXSessionEventData->serverResponseOpCode = %x\n", inOBEXSessionEventData->serverResponseOpCode );

	if( inOBEXSessionEventData->serverResponseOpCode != kOBEXResponseCodeSuccessWithFinalBit )
	{
		[self insertText:[NSString stringWithFormat:@"Request failed. Response code: 0x%x\n", inOBEXSessionEventData->serverResponseOpCode] withColor:[NSColor redColor]];
	}
		
	[self updateErrorText:[NSString stringWithFormat:@"Connected over RFCOMM/OBEX.\n" ]];	
	[ButtonOBEXConnectDisconnect setTitle:@"Disconnect"];
	[ButtonDeviceDisconnect setEnabled:TRUE];
	[ButtonSendGet setEnabled:TRUE];
	[ButtonSendPut setEnabled:TRUE];
	[ButtonSendSetPath setEnabled:TRUE];
}

//====================================================================================================================
//	OBEXGetHandler
//====================================================================================================================

- (void)OBEXGetHandler:(const OBEXGetCommandResponseData*)inOBEXSessionEventData
{
	CFDictionaryRef		dictionaryRef		= NULL;
	size_t				bodyDataReceived	= 0;
	CFDataRef			bodyDataRef			= NULL;
	CFDataRef			endOfBodyDataRef 	= NULL;

	// Done with the headers we used in the initial get command, so make sure we dump them.

	if( mGetHeadersDataRef )
	{
		CFRelease( mGetHeadersDataRef );
		mGetHeadersDataRef = NULL;
	}

	printf( DEBUG_NAME "[OBEXGetHandler] Entry. responsecode = %x\n", inOBEXSessionEventData->serverResponseOpCode );

	// handle data. Look for headers... most should be body headers.
		
	dictionaryRef = OBEXGetHeaders( inOBEXSessionEventData->headerDataPtr, inOBEXSessionEventData->headerDataLength );
	if( dictionaryRef )
	{
		// Get name of object.
					
		if( CFDictionaryGetCountOfKey( dictionaryRef, kOBEXHeaderIDKeyName ) > 0 )
		{
			// When we get the name, we're going to ask the user if they want this object. If not, we'll refuse it.
	
			CFStringRef theStringRef;
			
			theStringRef = (CFStringRef) CFDictionaryGetValue( dictionaryRef, kOBEXHeaderIDKeyName );
			if( theStringRef )
			{				
				[self updateErrorText:[NSString stringWithFormat:@"incoming file name is: %@\n", (NSString*) theStringRef ]];
			}
		}
		
		// Get length...
	
		if( CFDictionaryGetCountOfKey( dictionaryRef, kOBEXHeaderIDKeyLength ) > 0 )
		{
			CFStringRef theStringRef;
			
			// No need to release the stringref I get; when I destroy the dictionary, the contents
			// will be released as well...
			
			theStringRef = (CFStringRef) CFDictionaryGetValue( dictionaryRef, kOBEXHeaderIDKeyLength );
			if( theStringRef )
			{				
				CFDataRef dataRef;
				
				dataRef = (CFDataRef) CFDictionaryGetValue( dictionaryRef, kOBEXHeaderIDKeyLength );
				if( dataRef )
				{
					size_t lengthOfData;
					
					CFDataGetBytes( dataRef, CFRangeMake( 0, 4 ), (uint8_t*) &lengthOfData );
					[self updateErrorText:[NSString stringWithFormat:@"incoming file length is: %d\n", (int) lengthOfData ]];
				}
			}
		}
		
		// Get BODY Segments.
		
		if( CFDictionaryGetCountOfKey( dictionaryRef, kOBEXHeaderIDKeyBody ) > 0 )
		{
			bodyDataRef = (CFDataRef) CFDictionaryGetValue( dictionaryRef, kOBEXHeaderIDKeyBody );
			if( bodyDataRef )
			{
				bodyDataReceived += CFDataGetLength( bodyDataRef );
			}
		}
		
		// Get ENDOFBODY segments.
		
		if( CFDictionaryGetCountOfKey( dictionaryRef, kOBEXHeaderIDKeyEndOfBody ) > 0 )
		{
			endOfBodyDataRef = (CFDataRef) CFDictionaryGetValue( dictionaryRef, kOBEXHeaderIDKeyEndOfBody );
			if( endOfBodyDataRef )
			{
				bodyDataReceived += CFDataGetLength( endOfBodyDataRef );
			}
		}
	}

	switch( inOBEXSessionEventData->serverResponseOpCode )
	{
		case( kOBEXResponseCodeContinue ):
		{
			// Not done with the previous get, still more data is on the way. We're just going to ignore it, because
			// we're not too concerned with what they have to say in our app.
			
			printf( DEBUG_NAME "[OBEXGetHandler] kOBEXResponseCodeContinue. inOBEXSessionEventData->headerDataLength  = 0x%02x\n", (int) inOBEXSessionEventData->headerDataLength  );
			break;
		}
		case( kOBEXResponseCodeContinueWithFinalBit ):
		{
			OSStatus status;
			
			// Successful with the previous get, and it looks like we need to do another (because of the "continue")
			// part of the response code, so do another get request. We'll do this until a success with final bit command response
			// comes through.
			
			status = [mOBEXSession	OBEXGet:TRUE
									headers:NULL
									headersLength:0
									eventSelector:@selector( commandSentCallback: )
									selectorTarget:(void *)self
									refCon:self];
			printf( DEBUG_NAME "[OBEXGetHandler] kOBEXResponseCodeContinueWithFinalBit. inOBEXSessionEventData->headerDataLength  = 0x%02x\n", (int) inOBEXSessionEventData->headerDataLength  );
			break;
		}
		case( kOBEXResponseCodeSuccess ):
		{
			printf( DEBUG_NAME "[OBEXGetHandler] kOBEXResponseCodeSuccess. inOBEXSessionEventData->headerDataLength  = 0x%02x\n", (int) inOBEXSessionEventData->headerDataLength  );
			break;
		}
		case( kOBEXResponseCodeSuccessWithFinalBit ):
		{
			printf( DEBUG_NAME "[OBEXGetHandler] kOBEXResponseCodeSuccessWithFinalBit. inOBEXSessionEventData->headerDataLength  = 0x%02x\n", (int) inOBEXSessionEventData->headerDataLength  );
			break;
		}
		case( kOBEXResponseCodeBadRequest ):
		case( kOBEXResponseCodeBadRequestWithFinalBit ):
		{
			[self updateErrorText:[NSString stringWithFormat:@"Bad request (no such file on target).\n" ]];
			break;
		}
		default:
		{
			[self insertText:[NSString stringWithFormat:@"Request failed. Response code: 0x%x\n", inOBEXSessionEventData->serverResponseOpCode] withColor:[NSColor redColor]];
			break;
		}
	}
	
	// The body data is what we care about in the get operation, so let's save that stuff in a file.
	
	if( inOBEXSessionEventData->headerDataLength  > 0 )
	{
		FILE*				file			= NULL;
		size_t				bytesWritten	= 0;
		
		printf( DEBUG_NAME "[OBEXGetHandler] filename: %s\n", [[TextFieldNewFileName stringValue] cString] );
		file = fopen( [[TextFieldNewFileName stringValue] cString], "a" );
		
		if( file && bodyDataRef )
		{
			bytesWritten = fwrite( CFDataGetBytePtr( bodyDataRef ), 1, CFDataGetLength( bodyDataRef ), file );
			printf( DEBUG_NAME "[OBEXGetHandler] bytesWritten = %x.\n", (int) bytesWritten );
		}
		
		if( file && endOfBodyDataRef )
		{
			bytesWritten = fwrite( CFDataGetBytePtr( endOfBodyDataRef ), 1, CFDataGetLength( endOfBodyDataRef ), file );
			[self updateErrorText:[NSString stringWithFormat:@"[OBEXGetHandler] bytesWritten = %x.\n", (int) bytesWritten ]];
		}

		fflush( file );
		fclose( file );
		
		// make String with bytes and put in the Data textfield.
		if( bodyDataRef )
		{
			[self insertText:[NSString stringWithCString:((char*)CFDataGetBytePtr( bodyDataRef )) length:CFDataGetLength( bodyDataRef )] withColor:[NSColor redColor]];
		}
		
		if( endOfBodyDataRef )
		{
			[self insertText:[NSString stringWithCString:((char*)CFDataGetBytePtr( endOfBodyDataRef ))  length:CFDataGetLength( endOfBodyDataRef )] withColor:[NSColor redColor]];
		}
	}

	if( dictionaryRef ) CFRelease( dictionaryRef );
				
	return;
}

//====================================================================================================================
//	OBEXPutHandler
//====================================================================================================================

- (void)OBEXPutHandler:(const OBEXPutCommandResponseData*)inOBEXSessionEventData
{
	printf( DEBUG_NAME "[OBEXPutHandler] Entry. responsecode = %x\n", inOBEXSessionEventData->serverResponseOpCode );

	switch( inOBEXSessionEventData->serverResponseOpCode )
	{
		case( kOBEXResponseCodeContinue ):
		{
			[self insertText:@"Response is not complete! I don't think we handle this case.\n" withColor:[NSColor blueColor]];
			printf( DEBUG_NAME "[OBEXPutHandler] kOBEXResponseCodeContinue. dataLength = 0x%02x\n", (int) inOBEXSessionEventData->headerDataLength );
			break;
		}
		case( kOBEXResponseCodeContinueWithFinalBit ):
		{
			Boolean		isLastChunk = FALSE;
			float 		percentDone = (((float)mLastFileOffset/(float)mCurrentFileSize*(float)100));
			OBEXError	status;
			
			[PutProgressBar setDoubleValue:percentDone];
	
			// Get next bit of data from the file and send it.

			mTempPutDataBuffer = [self	getNextFileChunk:(NSFileHandle*)mCurrentFile
										optionalHeaderLength:CFDataGetLength( mPutHeadersDataRef )
										isLastChunk:&isLastChunk];

			status = [mOBEXSession	OBEXPut:isLastChunk
									headersData:(void*) NULL
									headersDataLength:0
									bodyData:(void*)[mTempPutDataBuffer bytes]
									bodyDataLength:[mTempPutDataBuffer length]
									eventSelector:@selector( commandSentCallback: )
									selectorTarget:(void *)self
									refCon:NULL];
			if( status != kOBEXSuccess )
			{
				[self resetButtons];
				[self updateErrorText:[NSString stringWithFormat:@"OBEXSessionPut failed. Error = 0x%x.\n", status ]];
			}

			[self insertText:@"." withColor:[NSColor blueColor]];
			printf( DEBUG_NAME "[OBEXPutHandler] kOBEXResponseCodeContinueWithFinalBit. inOBEXSessionEventData->headerDataLength  = 0x%02x\n", (int) inOBEXSessionEventData->headerDataLength  );
			break;
		}
		case( kOBEXResponseCodeSuccess ):
		{
			[PutProgressBar setDoubleValue:100];
			[self insertText:@"\nPut succeded.\n" withColor:[NSColor blueColor]];
			break;
		}
		case( kOBEXResponseCodeSuccessWithFinalBit ):
		{
			if ( mCurrentFile != nil )
			{
				[mCurrentFile release];
				mCurrentFile = NULL;
			}
			[PutProgressBar setDoubleValue:100];
			[self insertText:@"\nPut succeded.\n" withColor:[NSColor blueColor]];
			break;
		}
        case kOBEXResponseCodeNotAcceptable:		// Rejected - awww too bad
        {
			[self updateErrorText:@"[OBEXGetHandler] Put rejected: kOBEXResponseCodeNotAcceptable.\n"];
			break;
		}
		case kOBEXResponseCodeBadRequest:
        {
			[self updateErrorText:@"[OBEXGetHandler] Put rejected: kOBEXResponseCodeNotAcceptable.\n"];
			break;
		}
		default:
		{
			[self insertText:[NSString stringWithFormat:@"Request failed. Response code: 0x%x\n", inOBEXSessionEventData->serverResponseOpCode] withColor:[NSColor redColor]];
			break;
		}
	}
}

//====================================================================================================================
//	OBEXAbortHandler
//====================================================================================================================

- (void)OBEXAbortHandler:(const OBEXAbortCommandResponseData*)inOBEXSessionEventData
{
	printf( DEBUG_NAME "[OBEXAbortHandler] Entry. responsecode = %x\n", inOBEXSessionEventData->serverResponseOpCode );

	mAbortSent = FALSE;

	if( inOBEXSessionEventData->serverResponseOpCode != 0xa0 )
	{
		[self insertText:[NSString stringWithFormat:@"Request failed. Response code: 0x%x\n", inOBEXSessionEventData->serverResponseOpCode] withColor:[NSColor redColor]];
	}
	else
	{
		[self insertText:@"Abort successful.\n" withColor:[NSColor blueColor]];	
	}
}

//====================================================================================================================
//	OBEXDisconnectHandler
//====================================================================================================================

- (void)OBEXDisconnectHandler:(const OBEXDisconnectCommandResponseData*)inOBEXSessionEventData
{
	printf( DEBUG_NAME "[OBEXDisconnectHandler] Entry. responsecode = %x\n", inOBEXSessionEventData->serverResponseOpCode );

	if( inOBEXSessionEventData->serverResponseOpCode != 0xa0 )
	{
		[self insertText:[NSString stringWithFormat:@"Request failed. Response code: 0x%x\n", inOBEXSessionEventData->serverResponseOpCode] withColor:[NSColor redColor]];
	}
	
	[self updateErrorText:[NSString stringWithFormat:@"OBEX Disconnection complete.\n" ]];
	
	[ButtonOBEXConnectDisconnect setTitle:@"Connect"];
	[ButtonSendGet setEnabled:FALSE];
	[ButtonSendPut setEnabled:FALSE];
	[ButtonSendSetPath setEnabled:FALSE];
}

//====================================================================================================================
//	OBEXSetPathHandler
//====================================================================================================================

- (void)OBEXSetPathHandler:(const OBEXSetPathCommandResponseData*)inOBEXSessionEventData
{
	if( mSetPathHeadersDataRef )
	{
		CFRelease( mSetPathHeadersDataRef );
		mSetPathHeadersDataRef = NULL;
	}
	
	switch( inOBEXSessionEventData->serverResponseOpCode )
	{
		case( kOBEXResponseCodeContinue ):
		{
			[self updateErrorText:[NSString stringWithFormat:@"kOBEXResponseCodeContinue.\n" ]];
			break;
		}
		case( kOBEXResponseCodeContinueWithFinalBit ):
		{
			[self updateErrorText:[NSString stringWithFormat:@"kOBEXResponseCodeContinueWithFinalBit.\n" ]];
			break;
		}
		case( kOBEXResponseCodeSuccess ):
		{
			[self updateErrorText:[NSString stringWithFormat:@"kOBEXResponseCodeSuccess.\n" ]];
			break;
		}
		case( kOBEXResponseCodeSuccessWithFinalBit ):
		{
			[self updateErrorText:[NSString stringWithFormat:@"kOBEXResponseCodeSuccessWithFinalBit.\n" ]];
			break;
		}
		case( kOBEXResponseCodeBadRequest ):
		case( kOBEXResponseCodeBadRequestWithFinalBit ):
		{
			[self updateErrorText:[NSString stringWithFormat:@"Bad request (no such path on target).\n" ]];
			break;
		}
		default:
		{
			[self insertText:[NSString stringWithFormat:@"Request failed. Response code: 0x%x\n", inOBEXSessionEventData->serverResponseOpCode] withColor:[NSColor redColor]];
			break;
		}
	}
	
	printf( DEBUG_NAME "[OBEXSetPathHandler] Entry. responsecode = %x", inOBEXSessionEventData->serverResponseOpCode );
}

#pragma mark -
#pragma mark === Preferences handling ===

//=================================================================================================
//	saveUIState
//=================================================================================================

- (void)saveUIState
{
    NSUserDefaults		*userDefaults;
    
    userDefaults = [NSUserDefaults standardUserDefaults];

	[userDefaults setObject:[TextFieldPacketSize stringValue] forKey:TextFieldPacketSizeKey];
	[userDefaults setObject:[TextFieldAddress stringValue] forKey:TextFieldAddressKey];
	[userDefaults setObject:[TextFieldChannelID stringValue] forKey:TextFieldChannelIDKey];
    [userDefaults setObject:[TextFieldFileToGet stringValue] forKey:TextFieldFileToGetKey];
    [userDefaults setObject:[TextFieldFileToPut stringValue] forKey:TextFieldFileToPutKey];
    [userDefaults setObject:[TextFieldFileToPutNewName stringValue] forKey:TextFieldFileToPutNewNameKey];
    [userDefaults setObject:[TextFieldNewFileName stringValue] forKey:TextFieldNewFileNameKey];
    [userDefaults setObject:[TextFieldSetPath stringValue] forKey:TextFieldSetPathKey];

    
    [userDefaults synchronize];
    
}

//=================================================================================================
//	restoreUIState
//=================================================================================================

- (void)restoreUIState
{
    NSUserDefaults		*userDefaults;
    NSString			*tempString;

    userDefaults = [NSUserDefaults standardUserDefaults];

    tempString = [userDefaults objectForKey:TextFieldAddressKey];
    if(tempString)      [TextFieldAddress setStringValue:tempString];

    tempString = [userDefaults objectForKey:TextFieldChannelIDKey];
    if(tempString)      [TextFieldChannelID setStringValue:tempString];
	
	tempString = [userDefaults objectForKey:TextFieldPacketSizeKey];
    if(tempString)      [TextFieldPacketSize setStringValue:tempString];

    tempString = [userDefaults objectForKey:TextFieldFileToGetKey];
    if(tempString)      [TextFieldFileToGet setStringValue:tempString];

    tempString = [userDefaults objectForKey:TextFieldFileToPutKey];
    if(tempString)      [TextFieldFileToPut setStringValue:tempString];

    tempString = [userDefaults objectForKey:TextFieldFileToPutNewNameKey];
    if(tempString)      [TextFieldFileToPutNewName setStringValue:tempString];

    tempString = [userDefaults objectForKey:TextFieldNewFileNameKey];
    if(tempString)      [TextFieldNewFileName setStringValue:tempString];

    tempString = [userDefaults objectForKey:TextFieldSetPathKey];
    if(tempString)      [TextFieldSetPath setStringValue:tempString];

}

//=================================================================================================
//	awakeFromNib
//=================================================================================================

- (void)awakeFromNib
{
    [(NSApplication *)NSApp setDelegate:self];
    [self restoreUIState];
}

//=================================================================================================
//	applicationWillTerminate
//=================================================================================================

- (void)applicationWillTerminate:(NSNotification *)sender
{
    [self saveUIState];
}


@end
