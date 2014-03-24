/*
	File:		AppController.m
	Contains:	Sample code for using the OBEXFileTransferServices APIs available in the IOBluetooth Framework. This
				example uses the objective-C API for the session. See other examples for usage of the C API.
	Author:		Mathew Davidson
 
	Copyright (c) 2005 by Apple Computer, Inc., all rights reserved.
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


#import <Cocoa/Cocoa.h>

#import <IOBluetooth/Bluetooth.h>
#import <IOBluetooth/OBEX.h>
#import <IOBluetooth/objc/IOBluetoothDevice.h>
#import <IOBluetooth/objc/IOBluetoothSDPUUID.h>
#import <IOBluetooth/objc/IOBluetoothSDPDataElement.h>
#import <IOBluetooth/objc/OBEXFileTransferServices.h>
#import <IOBluetooth/objc/OBEXSession.h>
#import <IOBluetooth/objc/IOBluetoothOBEXSession.h>

#import <IOBluetoothUI/objc/IOBluetoothDeviceSelectorController.h>

#import "AppController.h"

#define Log( FMT, ARGS... )		NSLog(@ FMT, ## ARGS )

int ListingValuesSorter( id	object1, id object2, void* theContext );

enum
{
	kTestTypeOOP	= 1,
	kTestTypeFTP	= 2
};


@implementation AppController
//===========================================================================================================================
//	AppController
//===========================================================================================================================

- (id) init
{
	self = [super init];
		
	return self;
}


//===========================================================================================================================
- (void) dealloc
{	
	if( mFTPCurrentDirectoryObjects ){ [mFTPCurrentDirectoryObjects release]; }
	
	if( mTransferServices || [mTransferServices isConnected] )
	{
		[mTransferServices disconnect];
	}
	
	[super dealloc];
}

//===========================================================================================================================
- (void) finalize
{	
	if( mTransferServices || [mTransferServices isConnected] )
	{
		[mTransferServices disconnect];
	}
	
	[super finalize];
}



//===========================================================================================================================
//	awakeFromNib
//===========================================================================================================================
- (void) awakeFromNib
{	
	[mOPPProgressIndicator		setIndeterminate: FALSE];
	[mOPPFileStatusField		setStringValue: @"Done"];
	[mOPPFileTransferredField	setStringValue: @""];
	[mOPPFileRemainingField		setStringValue: @"" ];
}


//===========================================================================================================================
//	applicationWillFinishLaunching
//===========================================================================================================================
- (void) applicationWillFinishLaunching:(NSNotification *)notification;
{
    if ( IOBluetoothValidateHardware( nil ) != kIOReturnSuccess )
    {
        [NSApp terminate:self];
    }
}


//===========================================================================================================================
//
//===========================================================================================================================
- (NSTableView *)ftpTable
{
	return mFTPBrowseTable;
}


#pragma mark -
#pragma mark Connection 
//===========================================================================================================================
//	connect
//===========================================================================================================================
- (void) connect
{
    IOBluetoothDeviceSelectorController *	deviceSelector;
    NSArray *								devicesArray;
	IOBluetoothSDPServiceRecord *			record;
	IOBluetoothDevice *						device;
	
	Log("[connect] entry\n");
	
    // Get browser to get a service ref.
    deviceSelector = [IOBluetoothDeviceSelectorController deviceSelector];
    if(!deviceSelector)
	{
		Log("[connect] Err: no device selector\n");
		goto exit;
	}
    
	mTestType = ( [mTabView selectedTabViewItem] == [mTabView tabViewItemAtIndex: 0] );
	
	//
	//  OBJECT PUSH
	//
	if( mTestType == kTestTypeOOP )
	{
		[deviceSelector addAllowedUUID: [IOBluetoothSDPUUID uuid16: kBluetoothSDPUUID16ServiceClassOBEXObjectPush]];
		[deviceSelector setTitle: @"Send file"];
		[deviceSelector setPrompt: @"Send"];
		[deviceSelector setDescriptionText: @"Select a device to send file to"];
	}
	//
	//  FILE TRANSFER
	//
	else
	{
		[deviceSelector addAllowedUUID: [IOBluetoothSDPUUID uuid16: kBluetoothSDPUUID16ServiceClassOBEXFileTransfer]];
		[deviceSelector setTitle: @"Browse"];
		[deviceSelector setPrompt: @"Browse"];
		[deviceSelector setDescriptionText: @"Select a device to browse"];
	}
	
	// run the panel
    [deviceSelector setSearchAttributes:0];
	[deviceSelector	runModal];
	
    devicesArray = [deviceSelector getResults];
    if( !devicesArray )
	{
		Log("[connect] ERR: no device array\n");
		goto exit;
	}
	
	// get the selected device
	device = [devicesArray objectAtIndex:0];        
    if( !device )
	{
		Log("[connect] ERR: no device\n");
		goto exit;
	}
    
	//  Get the service record of the service we want to use
	//
	if( mTestType == kTestTypeOOP )
	{
		record = [device getServiceRecordForUUID:[IOBluetoothSDPUUID uuid16:kBluetoothSDPUUID16ServiceClassOBEXObjectPush]];
	}
	else
	{
		record = [device getServiceRecordForUUID:[IOBluetoothSDPUUID uuid16:kBluetoothSDPUUID16ServiceClassOBEXFileTransfer]];
	}
	
	//  Use the record to form an OBEX Session object
	//
	mOBEXSession = [IOBluetoothOBEXSession withSDPServiceRecord: record];
	[mOBEXSession retain];
	
	//  Send the OBEXSession off to FTS
	//
	mTransferServices = [OBEXFileTransferServices withOBEXSession: mOBEXSession];
	[mTransferServices retain];
	[mTransferServices setDelegate: self];
	
	
	// Update the UI buttons
	[mOPPSendFileButton setTitle:	@"Abort!"];
	[mOPPSendFileButton setAction:	@selector(abortAction:)];
	
	[mFTPConnectButton setTitle:	@"Disconnect"];
	[mFTPConnectButton setAction:	@selector(abortAction:)];
	
	//
	//  Connect to the service we specified when we did our device discovery
	//
	if( mTestType == kTestTypeOOP )
	{
		[mTransferServices connectToObjectPushService];
	}
	else
	{
		[mTransferServices connectToFTPService];
	}
	
exit:
		
		Log("[connect] exit\n");
	
	return;
}


//===========================================================================================================================
//	abortAction
//===========================================================================================================================
- (void) abortAction:(id)sender
{
	Log("[ABORT] we're aborting, weeee!!!.....\n");
	
	if( mTransferServices && [mTransferServices isConnected] )
	{
		if( mTestType == kTestTypeOOP )
		{
			[mTransferServices abort];
		}
		else
		{
			[mTransferServices disconnect];
		}
	}
}

//===========================================================================================================================
//	quit
//===========================================================================================================================
- (IBAction) quit:(id)sender
{
	Log(" Time to die.....\n");
	
	[NSApp terminate:self];
}



#pragma mark -
#pragma mark Actions 
//===========================================================================================================================
//	sendFileOPP
//===========================================================================================================================
- (IBAction) sendFileOPP:(id)sender
{	
	NSOpenPanel	* openPanel = nil;
	
	if( mTransferServices && [mTransferServices isConnected] )
	{
		Log("[sendOPP] ERR: still connected!\n"); 
		return;
	}
	
	openPanel = [NSOpenPanel openPanel]; 
	if( !openPanel ){ return; }
	
	[openPanel	setAllowsMultipleSelection:FALSE];
	[openPanel	setCanChooseFiles:TRUE];
	[openPanel	setCanChooseDirectories:FALSE];
	[openPanel	setPrompt:@"Send"];
	
	[openPanel beginSheetForDirectory: nil
								 file: nil
								types: nil
					   modalForWindow: mMainWindow
						modalDelegate: self
					   didEndSelector: @selector( sendOPPPanelDidEnd:returnCode:contextInfo: )
						  contextInfo: self];
}



//====================================================================================================================
//	sendOPPPanelDidEnd
//====================================================================================================================

- (void) sendOPPPanelDidEnd:(NSOpenPanel *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo
{	
	// Did they cancel?
	if( returnCode == NSCancelButton ){ return; }
	
	mOPPSelectedFile = [[[sheet filenames] lastObject] copy];	
	
	[self performSelector: @selector(connect)
			   withObject: nil
			   afterDelay: 0.1];
}




//===========================================================================================================================
//	connectToFTP
//===========================================================================================================================
- (IBAction) connectToFTP:(id)sender
{	
	[self connect];
}


//====================================================================================================================
//  getFile
//====================================================================================================================
- (IBAction) getFileFTP:(id)sender
{
	NSSavePanel *	savePanel		= nil;
	NSString *		dir				= nil;
	NSString *		file			= nil;
	int				row;
	
	if( ! mTransferServices || [mTransferServices isConnected] == FALSE )
	{
		Log("[get] ERR: no connection!\n"); 
		goto exit;
	}
	
	if( !mFTPCurrentDirectoryObjects )
	{
		Log("[get] ERR: no directory items!\n"); 
		goto exit;
	}
	
	row = [mFTPBrowseTable selectedRow];
	if( row < 0 || row >= [mFTPCurrentDirectoryObjects count] )
	{
		Log("[get] ERR: bad selection\n"); 
		goto exit;
	}

	savePanel = [NSSavePanel savePanel];
	if( !savePanel )
	{
		Log("[get] ERR: No save panel\n");
		goto exit;
	}
	
	file = [[mFTPCurrentDirectoryObjects objectAtIndex:row] objectForKey:@"name"];
	
	[savePanel beginSheetForDirectory: dir
								 file: file
					   modalForWindow: mMainWindow
						modalDelegate: self
					   didEndSelector: @selector( getFTPPanelDidEnd:returnCode:contextInfo: )
						  contextInfo: self];	
exit:
	Log("[get] exit\n");
	return;
}


//====================================================================================================================
//	getFTPPanelDidEnd
//====================================================================================================================
- (void) getFTPPanelDidEnd:(NSSavePanel *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo
{
	NSString *			chosenPath	= nil;
	NSString *			fileName	= nil;
	BOOL				conflict	= YES;
	NSFileManager *		fm			= nil;
	int					row;
	
	Log("[getFTPPanelDidEnd] entry.  Return: %d\n", returnCode );
	
	// Did they cancel?
	
	if( returnCode == NSCancelButton )
	{
		return;
	}
	
	conflict = FALSE;
	fm = [NSFileManager defaultManager];
	
	chosenPath = [sheet filename];
	Log("Final path: %s\n", [chosenPath UTF8String] );
	
	
	row = [mFTPBrowseTable selectedRow];
	if( row < 0 || row  >= [mFTPCurrentDirectoryObjects count] )
	{
		return;
	}

	fileName = [[mFTPCurrentDirectoryObjects objectAtIndex: row] objectForKey: @"name"];
	
	if( [fm fileExistsAtPath:chosenPath] )
	{
		int res;
		
		res = NSRunAlertPanel( @"Save", 
							   [NSString stringWithFormat:@"Selected files already exist at %@.  Replace them?", chosenPath], 
							   @"Replace",
							   @"Cancel", 
							   nil);
		
		if( res == NSAlertDefaultReturn )
		{
			[mTransferServices copyRemoteFile: fileName
								  toLocalPath: chosenPath];	
		}
	}
	else
	{
		[mTransferServices copyRemoteFile: fileName
							  toLocalPath: chosenPath];
	}
}



//====================================================================================================================
//	sendFile
//====================================================================================================================
- (IBAction) sendFileFTP:(id)sender
{
	NSOpenPanel	* openPanel = nil;
		
	if( ! mTransferServices || [mTransferServices isConnected] == FALSE )
	{
		Log("[send] ERR: no connection!\n"); 
		return;
	}
	
	openPanel = [NSOpenPanel openPanel]; 
	if( !openPanel ){ return; }
	
	[openPanel	setAllowsMultipleSelection:FALSE];
	[openPanel	setCanChooseFiles:TRUE];
	[openPanel	setCanChooseDirectories:FALSE];
	[openPanel	setPrompt:@"Send"];
	
	[openPanel beginSheetForDirectory: nil
								 file: nil
								types: nil
					   modalForWindow: mMainWindow
						modalDelegate: self
					   didEndSelector: @selector( sendFTPPanelDidEnd:returnCode:contextInfo: )
						  contextInfo: self];
}



//====================================================================================================================
//	openPanelDidEnd
//====================================================================================================================

- (void) sendFTPPanelDidEnd:(NSOpenPanel *)sheet returnCode:(int)returnCode contextInfo:(void *)contextInfo
{	
	// Did they cancel?
	
	if( returnCode == NSCancelButton ){ return; }
	
	[mTransferServices sendFile: [[sheet filenames] lastObject]];
}





#pragma mark -
#pragma mark FTS Handlers 
//====================================================================================================================
//	fileTransferServicesConnectionComplete:
//====================================================================================================================
- (void) fileTransferServicesConnectionComplete: (OBEXFileTransferServices*)inServices
										  error: (OBEXError)inError
{
	Log("[ConnectH]  entry - status: %d\n", inError);
	
	switch( inError )
	{
		case( kOBEXSuccess ):
		{						
			if( mTestType == kTestTypeOOP )
			{
				[mOPPProgressIndicator	setIndeterminate: TRUE];
				[mOPPProgressIndicator	startAnimation: self];
				[mOPPFileStatusField	setStringValue: @"Waiting for response..."];
				
				[mTransferServices sendFile: mOPPSelectedFile];
			}
			else
			{
				[mTransferServices changeCurrentFolderToRoot];
			}
			
			break;
		}
			
		default:
		{
			Log("[ConnectH]  We had an error: %d\n", inError );	
			break;
		}
	}
}


//====================================================================================================================
//	fileTransferServicesDisconnectionComplete:
//====================================================================================================================
- (void) fileTransferServicesDisconnectionComplete: (OBEXFileTransferServices*)inServices	
											 error: (OBEXError)inError
{
	Log("[Disconnect_H]  entry - err: %d\n", inError);
		
	switch( inError )
	{			
		case( kOBEXSuccess ):
		{			
			break;
		}
			
		case( kOBEXTimeoutError ):
		{
			Log("[Disconnect_H] ERR: Time-out\n");
			break;
		}
		
		case( kOBEXSessionTransportDiedError ):
		case( kOBEXSessionNoTransportError ):
		case( kOBEXSessionNotConnectedError ):
		{
			Log("[Disconnect_H] ERR: Transport %d\n", inError );	
			break;
		}
			
		default:
		{
			Log("[Disconnect_H] Defaulted: %d\n", inError );	
			break;
		}
	}
	
	[mOPPSendFileButton setTitle: @"Send File..."];
	[mOPPSendFileButton setAction: @selector(runTest:)];
	
	[mFTPConnectButton setTitle: @"Connect"];
	[mFTPConnectButton setAction: @selector(runTest:)];
	
	[mFTPCurrentDirectoryObjects removeAllObjects];
	[mFTPBrowseTable reloadData];
	
	[mTransferServices release];
	mTransferServices = nil;

	[[mOBEXSession getDevice] closeConnection];
	[mOBEXSession release];
	mOBEXSession = nil;
	
	if( inError )
	{
		Log("[Disconnect_H] We tried to disconnect but junk returned to us.\n");
	}	
}


//====================================================================================================================
//	fileTransferServicesSendFileProgress:
//====================================================================================================================
- (void) fileTransferServicesSendFileProgress:(OBEXFileTransferServices*)inServices			transferProgress:(NSDictionary*)inProgress
{		
	[mOPPProgressIndicator		setIndeterminate: TRUE];
	[mOPPFileTransferredField	setStringValue: [inProgress objectForKey: (NSString*)kFTSProgressTransferRateKey]];
	[mOPPFileStatusField		setStringValue: @"Transferring"];
	[mOPPFileRemainingField		setFloatValue: ([[inProgress objectForKey: (NSString*)kFTSProgressBytesTotalKey] intValue] - [[inProgress objectForKey: (NSString*)kFTSProgressBytesTransferredKey] floatValue]) ];
}


//====================================================================================================================
//	fileTransferServicesSendFileComplete:
//====================================================================================================================
- (void) fileTransferServicesSendFileComplete: (OBEXFileTransferServices*) inServices
										error: (OBEXError) inError
{
	Log("[Put_H] Entry. status = %d\n", inError );
	
	[mOPPProgressIndicator		setIndeterminate: FALSE];
	[mOPPFileStatusField		setStringValue: @"Done"];
	[mOPPFileTransferredField	setStringValue: @""];
	[mOPPFileRemainingField	setStringValue: @"" ];
	
	switch( inError )
	{
		case( kOBEXSuccess ):
		{
			Log("[SendComplete] All done....\n");
			if( mTestType == kTestTypeOOP )
			{
				[mTransferServices disconnect];
			}
			break;
		}
			
		case( kOBEXTimeoutError ):
		{
			Log(" [SendComplete] ERR: We've timed out, so lets bail out on those punks\n");
			[self fileTransferServicesDisconnectionComplete: mTransferServices
													  error: kOBEXSuccess];
			break;
		}
			
		case( kOBEXCancelledError ):
		case( kOBEXBadRequestError ):
		{
			Log("[SendComplete] ERR: they didn't accept it, bummer\n");
			
			[self fileTransferServicesSendFileComplete: mTransferServices
												 error: kOBEXSuccess];	
			break;
		}
		
		case( kOBEXGeneralError ):
		{
			Log("[SendComplete] ERR: transfer failed...\n");			
			[self fileTransferServicesDisconnectionComplete: mTransferServices
													  error: kOBEXSuccess];
			break;
		}
			
		default:
		{
			Log("[SendComplete] ERR: Defaulted (%d)\n", inError);
			break;
		}
	}
}


//====================================================================================================================
//	fileTransferServicesCopyRemoteFileProgress:
//====================================================================================================================
- (void) fileTransferServicesCopyRemoteFileProgress:(OBEXFileTransferServices*)inServices			transferProgress:(NSDictionary*)inProgress
{
	Log("[Get_H]: total:%d  tx:%d  rate:%f\n", [[inProgress objectForKey: (NSString*)kFTSProgressBytesTotalKey] intValue],
		[[inProgress objectForKey: (NSString*)kFTSProgressBytesTransferredKey] intValue],
		[[inProgress objectForKey: (NSString*)kFTSProgressTransferRateKey] floatValue] );
	
	
	[mOPPProgressIndicator		setIndeterminate: TRUE];
	[mOPPFileTransferredField	setStringValue: [inProgress objectForKey: (NSString*)kFTSProgressTransferRateKey]];
	[mOPPFileStatusField		setStringValue: @"Transferring"];
	[mOPPFileRemainingField		setFloatValue: ([[inProgress objectForKey: (NSString*)kFTSProgressBytesTotalKey] intValue] - [[inProgress objectForKey: (NSString*)kFTSProgressBytesTransferredKey] floatValue]) ];
}



//====================================================================================================================
//	fileTransferServicesCopyRemoteFileComplete:
//====================================================================================================================
- (void) fileTransferServicesCopyRemoteFileComplete:(OBEXFileTransferServices*)inServices			error:(OBEXError)inError
{
	[mOPPProgressIndicator		setIndeterminate: FALSE];
	[mOPPFileStatusField		setStringValue: @"Done"];
	[mOPPFileTransferredField	setStringValue: @""];
	[mOPPFileRemainingField	setStringValue: @"" ];
	
	
	switch( inError )
	{
		case( kOBEXSuccess ):
		{
			Log("[Get_H] Success\n");
			// [[NSWorkspace sharedWorkspace] noteFileSystemChanged: [NSHomeDirectory() stringByAppendingPathComponent: @"gauntlet-vcard.vcf"]];			
			break;
		}
			
		case( kOBEXGeneralError ):
		{
			Log("[Get_H] ERR: General Error\n");
			break;
		}		
			
		case( kOBEXTimeoutError ):
		{
			Log("[Get_H] ERR: Timed-out\n");
			break;
		}
			
		case( kOBEXBadRequestError ):
		{
			Log("[Get_H] ERR: Bad Request\n");
			break;
		}
			
		case( kOBEXCancelledError ):
		{
			Log("[Get_H] ERR: Cancelled\n");
			break;
		}
			
		default:
		{
			Log("[Get_H] Defaulted: %d\n", inError );
			break;
		}
	}
	
}



//====================================================================================================================
//	FTS_AbortHandler:
//====================================================================================================================
- (void) fileTransferServicesAbortComplete: (OBEXFileTransferServices*)inServices
									 error: (OBEXError)inError
{
	switch( inError )
	{
		case( kOBEXSuccess ):
		{
			Log("[Abort_H] Success!\n");
			break;
		}
			
		case( kOBEXTimeoutError ):
		{
			Log("[Abort_H] ERR: Timed-out\n");			
			break;
		}
			
		default:
		{
			Log("[Abort_H] Defaulted: %d\n", inError);
			break;
		}
	}

	[mTransferServices disconnect];
}



//====================================================================================================================
//	fileTransferServicesRetrieveFolderListingComplete:
//====================================================================================================================
- (void) fileTransferServicesRetrieveFolderListingComplete: (OBEXFileTransferServices*)inServices 
													 error: (OBEXError)inError 
												   listing: (NSArray*)inListing
{
	Log("[Listing_H]  entry - err: %d\n", inError );
	
	if( inError == kOBEXSuccess )
	{
		[self listFilesInBrowserFromList: inListing];
	}

}

//====================================================================================================================
//	fileTransferServicesCreateFolderComplete:
//====================================================================================================================
- (void) fileTransferServicesCreateFolderComplete: (OBEXFileTransferServices*)inServices
											error: (OBEXError)inError
										   folder: (NSString*)inFolderName
{	
	Log("[Create_H]  entry - status: %d\n", inError);
	
	if( inError == kOBEXSuccess )
	{
		[mTransferServices retrieveFolderListing];
	}

}


//====================================================================================================================
//	fileTransferServicesPathChangeComplete:
//====================================================================================================================
- (void) fileTransferServicesPathChangeComplete: (OBEXFileTransferServices*)inServices
										  error: (OBEXError)inError 
									  finalPath: (NSString*)inPath
{
	Log("[Path_H]  entry - err: %d\n", inError );
	
	if( inError == kOBEXSuccess )
	{
		[mTransferServices retrieveFolderListing];
	}
}



#pragma mark -
#pragma mark TableViewDataSource Stuff
//====================================================================================================================
//	numberOfRowsInTableView:
//====================================================================================================================
- (int) numberOfRowsInTableView:(NSTableView *)tv
{	
	return	[mFTPCurrentDirectoryObjects count];
}

//====================================================================================================================
//	tableView:
//====================================================================================================================
- (id) tableView:(NSTableView *)tv objectValueForTableColumn:(NSTableColumn *)tc row:(int)row
{
	id object = nil;
	
	if( !mFTPCurrentDirectoryObjects || row == -1 ){ goto exit; }
	if( row >= [mFTPCurrentDirectoryObjects count]){ goto exit; } 
	
	object = [mFTPCurrentDirectoryObjects objectAtIndex: row];		
	
exit:
		
	return( object );
}


//====================================================================================================================
//	tableView:willDisplayCell:forTableColumn:row:
//====================================================================================================================
- (void) tableView:(NSTableView *)tv willDisplayCell:(id)cell forTableColumn:(NSTableColumn *)column row:(int)rowIndex
{
	NSMutableString *	sizeString;
	NSMutableString *	tempString;
	int					fileType	= 0;
	NSNumber *			tempNumber;
	id object;
	
	if( tv != mFTPBrowseTable )
	{
		return;
	}
	if( !mFTPCurrentDirectoryObjects || rowIndex == -1 ){ goto exit; }

	
	NS_DURING

	object = [mFTPCurrentDirectoryObjects objectAtIndex: rowIndex];
	tempNumber = (NSNumber*) [(NSDictionary*)object objectForKey: (NSString*)kFTSListingTypeKey];
	if( tempNumber )
	{
		fileType = [tempNumber intValue];
	}
	else
	{
		fileType = kFTSFileTypeFile;
	}

	[cell setFont:[NSFont systemFontOfSize:10]];

	if( [(NSString*)[column identifier] compare:@"name"] == NSOrderedSame )
	{   // FILE NAME
		[cell setStringValue: [(NSDictionary*)object objectForKey: (NSString*)kFTSListingNameKey]];
	}
	else if( [(NSString*)[column identifier] compare:@"size"] == NSOrderedSame )
	{	// FILE SIZE
		tempString = [NSMutableString stringWithCapacity:10];
		sizeString = [(NSDictionary*)object objectForKey: (NSString*)kFTSListingSizeKey];
		
		if( !sizeString || ( fileType == kFTSFileTypeFolder ) )
		{
			[tempString appendString:@"-folder-"];
		}
		else if( sizeString && [sizeString length] > 0 )
		{
			[tempString	appendString: sizeString];
		}
		
		[cell setStringValue:(NSString*)tempString];
	}
			
	NS_HANDLER
				
	NS_ENDHANDLER
	
exit:

	return;
}


#pragma mark -
#pragma mark TabView
//====================================================================================================================
- (BOOL)tabView:(NSTabView *)tabView shouldSelectTabViewItem:(NSTabViewItem *)tabViewItem
{
	if( mTransferServices && [mTransferServices isConnected] )
	{
		return NO;
	}
	else
	{
		return YES;
	}	
}




#pragma mark -
#pragma mark Utility 
//====================================================================================================================
//	listFilesInBrowserFromList
//====================================================================================================================
- (void) listFilesInBrowserFromList:(NSArray*)inObjects
{
	Log("listing...\n");
	
	// make sure we have a store for our browser data.
	
	if( mFTPCurrentDirectoryObjects )
	{
		[mFTPCurrentDirectoryObjects release];
		mFTPCurrentDirectoryObjects = nil;
	}
	
	// parse folder listing.
	
	mFTPCurrentDirectoryObjects = [[NSMutableArray alloc] initWithArray: inObjects];
	require( mFTPCurrentDirectoryObjects, exit );
	
	// Sort the dictionary.
	
	[mFTPBrowseTable selectColumn:0 byExtendingSelection:FALSE];
	[mFTPCurrentDirectoryObjects retain];
	[mFTPCurrentDirectoryObjects sortUsingFunction:ListingValuesSorter context:self];
	
exit:
		
	[mFTPBrowseTable reloadData];
	[mFTPBrowseTable selectRow:0 byExtendingSelection:FALSE];
}



@end





#pragma mark -

//====================================================================================================================
//	ListingValuesSorter
//====================================================================================================================

int ListingValuesSorter( id	object1, id object2, void* theContext ) 
{
	NSString*	compareString1	= nil;
	NSString*	compareString2	= nil;
	int			sortingColumn	= 0;
	int			returnValue		= 0;
	int			fileType1		= 0;
	int			fileType2		= 0;
	
	
	if( !object1 || !object2 || !theContext )
	{
		return( NSOrderedSame );
	}
	sortingColumn = [[((AppController*)theContext) ftpTable] selectedColumn];
	
	if( sortingColumn != 1 ){ goto defaultCompareExit; }
	
	// try size comparison.
	
	fileType1 = [(NSNumber*)[(NSDictionary*)object1 objectForKey: (NSString*)kFTSListingSizeKey] intValue];
	fileType2 = [(NSNumber*)[(NSDictionary*)object2 objectForKey: (NSString*)kFTSListingSizeKey] intValue];
	
	if( fileType1 == fileType2 )
	{
		goto defaultCompareExit;
	}
	else
	{		
		if( fileType1 > fileType2 )
		{
			returnValue = NSOrderedDescending;
		}
		else
		{
			returnValue = NSOrderedAscending;
		}
	}
	
	return( returnValue );
	
defaultCompareExit:
		
	// name column = 0
	{
		// Sort alpha by default.
		compareString1 = [(NSDictionary*)object1 objectForKey: (NSString*)kFTSListingNameKey];
		compareString2 = [(NSDictionary*)object2 objectForKey: (NSString*)kFTSListingNameKey];
		
		if( !compareString1 || !compareString2 ) return( NSOrderedSame );
		
		returnValue = [compareString1	compare:compareString2];
	}
	
	return( returnValue );
}




