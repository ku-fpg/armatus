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

#import <unistd.h>

#import <Foundation/Foundation.h>
#import <Cocoa/Cocoa.h>
#import <CoreFoundation/CoreFoundation.h>
#import <CoreServices/CoreServices.h>

#import <IOBluetooth/objc/IOBluetoothDeviceInquiry.h>
#import <IOBluetooth/IOBluetoothUserLib.h>
#import <IOBluetoothUI/IOBluetoothUIUserLib.h>

#import "BluetoothExplorerWindow.h"

//===========================================================================================================================
// Globals
//===========================================================================================================================

NSSize	gCellSize;
NSSize	gCellImageSize;

//===========================================================================================================================
//	BluetoothExplorerWindow
//===========================================================================================================================

@implementation BluetoothExplorerWindow

#if 0
#pragma mark -
#pragma mark === NSApp Delegate methods and routines ===
#endif

//===========================================================================================================================
//	- (void)awakeFromNib
//===========================================================================================================================

- (void)awakeFromNib
{
	gCellSize 		= NSMakeSize( 329, 70 );
	gCellImageSize	= NSMakeSize( 48, 48 );

    [NSApp	setDelegate:self];
    
    // Setup the button matrix to have zero buttons since we cannot do this in IB.
    [_buttonMatrix removeColumn:0];
    [_buttonMatrix renewRows:0 columns:1];
    [_buttonMatrix sizeToCells];
	[_buttonMatrix setCellSize:gCellSize];
	[_buttonMatrix setDoubleAction:@selector( deviceListDoubleAction )];
	
    [_matrixView setNeedsDisplay:TRUE];

	if( IOBluetoothValidateHardware( nil ) != kIOReturnSuccess )
	{
		[NSApp terminate:self];
	}
}


#if 0
#pragma mark -
#pragma mark === IBAction Actions ===
#endif

//===========================================================================================================================
// clearFoundDevices
//===========================================================================================================================

-(IBAction)clearFoundDevices:(id)sender
{
	int numberOfRows = [_buttonMatrix numberOfRows];

	while( numberOfRows > 0 )
	{
		[_buttonMatrix removeRow:0];
	
		--numberOfRows;
	}

	[_foundDevices removeAllObjects];
	[_foundDevices release];
	_foundDevices = NULL;
}

//===========================================================================================================================
//	- (IBAction)handleSearchButton:(id)sender
//===========================================================================================================================

- (IBAction) handleSearchButton:(id)sender
{
  
	IOReturn ret;
	
	if( !_busy )
	{
		if( IOBluetoothLocalDeviceAvailable() == FALSE )
		{
			return;
		}
		
		[_searchButton 	setEnabled:TRUE];
		ret = [self startInquiry];
	}
	else
	{
		[_searchButton 	setEnabled:FALSE];
		ret = [self stopInquiry];
	}	
}

#if 0
#pragma mark -
#pragma mark === Start and Stop Inquiry ===
#endif

//===========================================================================================================================
// startInquiry
//===========================================================================================================================

-(IOReturn)startInquiry
{
	IOReturn	status;

	[self	stopInquiry];

	_inquiry = [IOBluetoothDeviceInquiry	inquiryWithDelegate:self];
	
	status = [_inquiry	start];
	if( status == kIOReturnSuccess )
	{
		[_inquiry	retain];
		[_progressBar startAnimation:self];
		[_searchButton 	setTitle:@"Stop"];

		_busy = TRUE;
	}
	else
	{
		[_messageText setObjectValue:@"Idle (Search Failed)."];
	}
	
	return( status );
}

//===========================================================================================================================
// stopInquiry
//===========================================================================================================================
- (IOReturn) stopInquiry
{
	IOReturn ret = kIOReturnNotOpen;
	
	if( _inquiry )
	{
		ret = [_inquiry stop];
		[_inquiry release];
		_inquiry = nil;
	}
	
	return ret;
}

#if 0
#pragma mark -
#pragma mark === IOBluetoothDeviceInquiry Delegate Messages ===
#endif

//===========================================================================================================================
// deviceInquiryStarted
//===========================================================================================================================

- (void)	deviceInquiryStarted:(IOBluetoothDeviceInquiry*)sender
{
	[_messageText 	setObjectValue:@"Searching for Devices..."];
	[_progressBar 	startAnimation:self];
}

//===========================================================================================================================
// deviceInquiryDeviceFound
//===========================================================================================================================

- (void)	deviceInquiryDeviceFound:(IOBluetoothDeviceInquiry*)sender	device:(IOBluetoothDevice*)device
{
	[self addDeviceToList:device];
	[_messageText setObjectValue:[NSString stringWithFormat:@"Found %d devices...", [[sender foundDevices] count]]];
}

//===========================================================================================================================
// deviceInquiryUpdatingDeviceNamesStarted
//===========================================================================================================================

- (void)	deviceInquiryUpdatingDeviceNamesStarted:(IOBluetoothDeviceInquiry*)sender	devicesRemaining:(int)devicesRemaining
{
	[_messageText setObjectValue:[NSString stringWithFormat:@"Refreshing %d device names...", devicesRemaining]];
}

//===========================================================================================================================
// deviceInquiryDeviceNameUpdated
//===========================================================================================================================

- (void)	deviceInquiryDeviceNameUpdated:(IOBluetoothDeviceInquiry*)sender	device:(IOBluetoothDevice*)device devicesRemaining:(int)devicesRemaining
{
	[_messageText setObjectValue:[NSString stringWithFormat:@"Refreshing %d device names...", devicesRemaining]];
	
	[self	updateDeviceInfoInList:device];
}

//===========================================================================================================================
// deviceInquiryComplete
//===========================================================================================================================

- (void)	deviceInquiryComplete:(IOBluetoothDeviceInquiry*)sender	error:(IOReturn)error	aborted:(BOOL)aborted
{
	if( aborted )
	{
		[_messageText	setObjectValue:@"Idle (inquiry stopped)."];
	}
	else
	{
		[_messageText	setObjectValue:@"Idle (inquiry complete)."];
	}

	[_progressBar 	stopAnimation:self];
	[_searchButton 	setTitle:@"Search"];
	[_searchButton 	setEnabled:TRUE];

	_busy = FALSE;
}

#if 0
#pragma mark -
#pragma mark === UI Stuff ===
#endif

//===========================================================================================================================
//	deviceListDoubleAction
//===========================================================================================================================

- (void)deviceListDoubleAction
{
	NSBeep();
}

//===========================================================================================================================
//	addDeviceToList
//===========================================================================================================================

-(void)addDeviceToList:(IOBluetoothDevice*)inDevice
{
    id								newButton;
	const BluetoothDeviceAddress*	addressPtr			= [inDevice getAddress];
    id 								buttonIcon			= NULL;
    NSString*						deviceCODString		= nil;
    NSString*						deviceAddressString	= nil;
	NSString*						deviceNameString	= [inDevice getName];
	NSString*						connectionInfo		= nil;

	// Get the device address and deal with the name.

	if( addressPtr )
	{
		deviceAddressString = [NSString stringWithFormat:@"%02x-%02x-%02x-%02x-%02x-%02x",	addressPtr->data[0],
																							addressPtr->data[1],
																							addressPtr->data[2],
																							addressPtr->data[3],
																							addressPtr->data[4],
																							addressPtr->data[5]];
	}
	
	if( !deviceNameString )
	{
		deviceNameString = @"<Name not yet known>";
	}
	
	// Make sure we don't already have this device in the list.
	
	if( ![self saveNewDeviceIfAcceptable:inDevice] )
	{
		// Already have seen it. Bail.
	
		return;
	}
	
	// Create COD info string.

	deviceCODString = [NSString stringWithFormat:@"Major Class: %@ (0x%02x)\nMinor Class: %@ (0x%02x)",
													GetStringForMajorCOD( [inDevice getDeviceClassMajor] ),
													[inDevice getDeviceClassMajor],
													GetStringForMinorCOD( [inDevice getDeviceClassMajor], [inDevice getDeviceClassMinor] ),
													[inDevice getDeviceClassMinor]];
	
	// If there is a connection in place shows the connection handle:
	if ( [inDevice isConnected] )
	{
		connectionInfo =  [NSString stringWithFormat:@"Connected, Connection Handle 0x%04x", [inDevice getConnectionHandle]];
	}
	else
	{
		connectionInfo =  [NSString stringWithFormat:@"Not Connected"];
	}

	
	// load up an image for the class of device which we have found.
	
	buttonIcon = [NSImage imageNamed:@"BluetoothLogo.tiff"];
	[buttonIcon setScalesWhenResized:TRUE];
	[buttonIcon setSize:gCellImageSize];

	// Make space for and get a button for the new device.
	
	[_buttonMatrix addRow];	
	newButton = [_buttonMatrix cellAtRow:[_buttonMatrix numberOfRows]-1 column:0];
	if( !newButton ) return;

	// Set the button's attributes.

	[newButton setImage:buttonIcon];
	[newButton setTitle:[NSString stringWithFormat:@"%@ / %@\n%@\n%@", [deviceAddressString uppercaseString], deviceNameString, deviceCODString, connectionInfo]];
	[newButton setTag:(int)inDevice];

	// make it the right type of button and update the display.
	
	[_buttonMatrix sizeToCells];
	[_matrixView setNeedsDisplay:TRUE];
}

//===========================================================================================================================
// updateDeviceInfoInList
//===========================================================================================================================

-(void)updateDeviceInfoInList:(IOBluetoothDevice *)inDevice
{
	id button = [_buttonMatrix cellWithTag:(int)inDevice];
	if( button )
	{
		NSString* deviceCODString 					= nil;
		const BluetoothDeviceAddress*	addressPtr	= [inDevice getAddress];
		NSString* deviceAddressString				= nil;
		NSString* name								= [inDevice getName];
		NSString* connectionInfo = nil;

		deviceCODString = [NSString stringWithFormat:@"Major Class: %@ (0x%02x)\nMinor Class: %@ (0x%02x)",
													GetStringForMajorCOD( [inDevice getDeviceClassMajor] ),
													[inDevice getDeviceClassMajor],
													GetStringForMinorCOD( [inDevice getDeviceClassMajor], [inDevice getDeviceClassMinor] ),
													[inDevice getDeviceClassMinor]];
                                                                                                        
		// If there is a connection in place shows the connection handle:
		if ( [inDevice isConnected] )
		{
			connectionInfo =  [NSString stringWithFormat:@"Connected, Connection Handle 0x%04x", [inDevice getConnectionHandle]];
		}
		else
		{
			connectionInfo =  [NSString stringWithFormat:@"Not Connected"];
		}

		// Get the device address and deal with the name.

		if( addressPtr )
		{
			deviceAddressString = [NSString stringWithFormat:@"%02x-%02x-%02x-%02x-%02x-%02x",	addressPtr->data[0],
																								addressPtr->data[1],
																								addressPtr->data[2],
																								addressPtr->data[3],
																								addressPtr->data[4],
																								addressPtr->data[5]];
		}
			
		if( !deviceAddressString )
		{
			deviceAddressString = @"Could not be retrieved.";
		}
		
		[button setTitle:[NSString stringWithFormat:@"%@ / %@\n%@\n%@", [deviceAddressString uppercaseString], name, deviceCODString, connectionInfo]];
		[_buttonMatrix setNeedsDisplay:TRUE];
	}
	else
	{
		NSLog( @"Nope, tag could not be found in matrix" );
	}
}

//===========================================================================================================================
//	saveNewDeviceIfAcceptable
//===========================================================================================================================

-(BOOL)saveNewDeviceIfAcceptable:(IOBluetoothDevice*)inNewDevice
{
	NSEnumerator*					enumerator;
	IOBluetoothDevice*				tmpDevice;
	const BluetoothDeviceAddress* 	newDeviceAddress = [inNewDevice getAddress];

	if( !_foundDevices )
	{
		_foundDevices = [[NSMutableArray alloc] initWithCapacity:1];
		if( !_foundDevices ) return( FALSE );
		[_foundDevices retain];
	}
	
	// walk the devices in the array.
	
	enumerator = [_foundDevices objectEnumerator];
	if( enumerator )
	{
		const BluetoothDeviceAddress* tempAddress = NULL;
			
		while( (tmpDevice = [enumerator nextObject]) )
		{
			tempAddress = [tmpDevice getAddress];
			
			if( memcmp( newDeviceAddress, tempAddress, sizeof( BluetoothDeviceAddress ) ) == 0 )
			{
				// Already have it.
				return( FALSE );
			}
		}
	}
	
	[_foundDevices addObject:inNewDevice];
	
	// Return that we haven't seen it.
	
	return( TRUE );
}

@end

#if 0
#pragma mark -
#pragma mark === C Stuff ===
#endif

//===========================================================================================================================
//	GetStringForMajorCOD
//===========================================================================================================================

NSString* GetStringForMajorCOD( BluetoothDeviceClassMajor inDeviceClassMajor )
{	
    switch( inDeviceClassMajor )
    {
        case( kBluetoothDeviceClassMajorMiscellaneous ):
		{
            return( @"Miscellaneous" );
            break;
		}           
        case( kBluetoothDeviceClassMajorComputer ):
		{
            return( @"Computer" );
			break;
		}
        case( kBluetoothDeviceClassMajorPhone ):
		{
            return( @"Phone" );
			break;
		}
        case( kBluetoothDeviceClassMajorLANAccessPoint ):
		{
            return( @"LAN Access Point" );
			break;
		}
		case( kBluetoothDeviceClassMajorAudio ):
		{
            return( @"Audio" );
			break;
		}
		case( kBluetoothDeviceClassMajorPeripheral ):
		{
            return( @"Peripheral" );
            break;
		}
		case( kBluetoothDeviceClassMajorImaging ):
		{
            return( @"Imaging" );
            break;
		}
    }
	
	return( @"Unclassified" );
}

//===========================================================================================================================
//	GetStringForMinorCOD
//===========================================================================================================================

NSString* GetStringForMinorCOD( BluetoothDeviceClassMajor inDeviceClassMajor, BluetoothDeviceClassMajor inDeviceClassMinor )
{	
    switch( inDeviceClassMajor )
    {
        case( kBluetoothDeviceClassMajorMiscellaneous ):
		{
            return( [NSString stringWithFormat:@"Unclassified",inDeviceClassMinor] );
            break;
		}           
        case( kBluetoothDeviceClassMajorComputer ):
		{
            if( inDeviceClassMinor == 0 ) return(  @"Unclassified" );
            if( inDeviceClassMinor == 1 ) return(  @"Desktop Workstation" );
            if( inDeviceClassMinor == 2 ) return(  @"Server" );
            if( inDeviceClassMinor == 3 ) return(  @"Laptop" );
            if( inDeviceClassMinor == 4 ) return(  @"Handheld" );
            if( inDeviceClassMinor == 5 ) return(  @"Palmsized" );
            if( inDeviceClassMinor == 6 ) return(  @"Wearable" );
			break;
		}
        case( kBluetoothDeviceClassMajorPhone ):
		{
			if( inDeviceClassMinor == 0 ) return(  @"Unclassified" );
            if( inDeviceClassMinor == 1 ) return(  @"Cellular" );
            if( inDeviceClassMinor == 2 ) return(  @"Cordless" );
            if( inDeviceClassMinor == 3 ) return(  @"SmartPhone" );
            if( inDeviceClassMinor == 4 ) return(  @"Wired Modem or Voice Gateway" );
            if( inDeviceClassMinor == 5 ) return(  @"Common ISDN Access" );
			break;
		}
        case( kBluetoothDeviceClassMajorLANAccessPoint ):
		{
			if( inDeviceClassMinor == 0 ) return(  @"0 used" );
            if( inDeviceClassMinor == 1 ) return(  @"1-17 used" );
            if( inDeviceClassMinor == 2 ) return(  @"18-33 used" );
            if( inDeviceClassMinor == 3 ) return(  @"34-50 used" );
            if( inDeviceClassMinor == 4 ) return(  @"51-67 used" );
            if( inDeviceClassMinor == 5 ) return(  @"68-83 used" );
            if( inDeviceClassMinor == 6 ) return(  @"84-99 used" );
            if( inDeviceClassMinor == 7 ) return(  @"No Service" );
			break;
		}
		case( kBluetoothDeviceClassMajorAudio ):
		{
			if( inDeviceClassMinor == 0 ) return(  @"Unclassified" );
			if( inDeviceClassMinor == 1 ) return(  @"Headset" );
			if( inDeviceClassMinor == 2 ) return(  @"Hands Free" );
			if( inDeviceClassMinor == 3 ) return(  @"Reserved 1" );
			if( inDeviceClassMinor == 4 ) return(  @"Microphone" );
			if( inDeviceClassMinor == 5 ) return(  @"Loudspeaker" );
			if( inDeviceClassMinor == 6 ) return(  @"Headphones" );
			if( inDeviceClassMinor == 7 ) return(  @"Portable" );
			if( inDeviceClassMinor == 8 ) return(  @"Car" );
			if( inDeviceClassMinor == 9 ) return(  @"Set-top Box" );
			if( inDeviceClassMinor == 10 ) return(  @"HiFi" );
			if( inDeviceClassMinor == 11 ) return(  @"VCR" );
			if( inDeviceClassMinor == 12 ) return(  @"Video Camera" );
			if( inDeviceClassMinor == 13 ) return(  @"CamCorder" );
			if( inDeviceClassMinor == 14 ) return(  @"Video Monitor" );
			if( inDeviceClassMinor == 15 ) return(  @"Video Display and Loudspeaker" );
			if( inDeviceClassMinor == 16 ) return(  @"Conferencing" );
			if( inDeviceClassMinor == 17 ) return(  @"Reserved2" );
			if( inDeviceClassMinor == 18 ) return(  @"Gaming Toy" );
			break;
		}
		case( kBluetoothDeviceClassMajorPeripheral ):
		{
			if( inDeviceClassMinor == 0 ) return(  @"Unclassified" );
            break;
		}
		case( kBluetoothDeviceClassMajorImaging ):
		{
			if( inDeviceClassMinor == 0 ) return(  @"Unclassified" );
            break;
		}
    }
	
	return(  @"Unclassified" );
}


