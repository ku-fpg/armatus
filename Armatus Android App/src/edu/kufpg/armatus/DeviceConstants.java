package edu.kufpg.armatus;

import android.content.Context;
import android.os.Build;
import android.os.Environment;
import android.support.annotation.NonNull;

public final class DeviceConstants {
	
	/** The directory where any persistent data should be saved. */
	public static final String CACHE_DIR = Environment.getExternalStorageDirectory().getPath() + "/data/armatus";

	/** The package name as specified in the Android Manifest file. */
	public static String PACKAGE_NAME;
	
	/** The current device's manufacturer and product name. */
	public static final String DEVICE_NAME = Build.MANUFACTURER + ' ' + Build.PRODUCT;
	
	private DeviceConstants() {}
	
	static void init(@NonNull final Context context) {
        Context c = context.getApplicationContext();
        if (c != null) {
            PACKAGE_NAME = c.getPackageName();
        } else {
            throw new IllegalArgumentException("Application Context is null");
        }
	}

}
