//
//  main.m
//  DeveloperSCOExample
//

#import <Cocoa/Cocoa.h>
#import <IOBluetooth/IOBluetoothUserLib.h>
#import <IOBluetooth/IOBluetoothUtilities.h>


//===================================================================================================
// Globals
//===================================================================================================

IOBluetoothObjectID		gObjectID = 0;
BluetoothDeviceAddress	gBDADDR;

//===================================================================================================
void SetGlobalsFromArguments( int argc, const char *argv[] )
{
    int i;
    
    for ( i = 1; i < argc; i++ )
    {
        const char *arg;
        
        arg = argv[i];
        
		NSLog(@"[main] arg[%d] = %s\n", i, arg);
		
		//  BDADDR
		//
        if ( ( arg != NULL ) && ( strcmp( arg, "BD_ADDR") == 0 ) )
        {
            if ( i < ( argc - 1 ) )
            {
				NSString * addr = [NSString stringWithCString: argv[i+1]];
				
				IOBluetoothNSStringToDeviceAddress( addr , &gBDADDR );
            }            
        }
	}
}


//===================================================================================================
//   M a i n
//===================================================================================================
int main(int argc, char *argv[])
{
	NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init];

	SetGlobalsFromArguments( argc, (const char **)argv );
	
	gObjectID = IOBluetoothGetObjectIDFromArguments( argc, (const char **)argv );

#if defined( MAC_OS_X_VERSION_10_5 )
	[pool drain];
#endif
	[pool release];

    return NSApplicationMain(argc,  (const char **) argv);
}
