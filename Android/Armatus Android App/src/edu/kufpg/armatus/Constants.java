package edu.kufpg.armatus;

import android.content.Context;
import android.os.Build;
import android.os.Environment;

public final class Constants {
	
	/** The directory where any persistent data should be saved. */
	public static final String CACHE_DIR = Environment.getExternalStorageDirectory().getPath() + "/data/armatus";

	/** The package name as specified in the Android Manifest file. */
	public static String PACKAGE_NAME;
	
	/** The current device's manufacturer and product name. */
	public static String DEVICE_NAME = Build.MANUFACTURER + " " + Build.PRODUCT;
	
	private Constants() {}
	
	static void initConstants(Context context) {
		PACKAGE_NAME = context.getApplicationContext().getPackageName();
	}

}
