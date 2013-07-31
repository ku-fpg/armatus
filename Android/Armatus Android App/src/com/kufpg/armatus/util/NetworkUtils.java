package com.kufpg.armatus.util;

import android.annotation.SuppressLint;
import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Build;
import android.provider.Settings;

/**
 * Utility class containing methods that detail the network state of the device.
 */
public class NetworkUtils {
	/** Monitors network connections. */
	private static ConnectivityManager mConMan;
	
	/** Contains information about the current network connection (if there is one). */
	private static NetworkInfo mNetworkInfo;

	private NetworkUtils() {}
	
	/**
	 * Returns whether the device's airplane mode is on.
	 * @param context The {@link Context} to use.
	 * @return <code>true</code> if airplane mode is on.
	 */
	@SuppressLint({ "NewApi", "InlinedApi" })
	@SuppressWarnings("deprecation")
	public static boolean isAirplaneModeOn(Context context) {
		if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR1) {
	        return Settings.System.getInt(context.getContentResolver(), 
	                Settings.System.AIRPLANE_MODE_ON, 0) != 0;          
	    } else {
	        return Settings.Global.getInt(context.getContentResolver(), 
	                Settings.Global.AIRPLANE_MODE_ON, 0) != 0;
	    }
	}

	/**
	 * Returns whether the device's Wi-Fi is connected.
	 * @param context The {@link Context} to use.
	 * @return <code>true</code> if Wi-Fi is connected.
	 */
	public static boolean isWifiConnected(Context context) {
		if (mConMan == null) {
			mConMan = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
		}
		if (mNetworkInfo == null) {
			mNetworkInfo = mConMan.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
		}
		if (mNetworkInfo == null)
			return false;
		if (!mNetworkInfo.isConnected())
			return false;
		if (!mNetworkInfo.isAvailable())
			return false;
		return true;
	}

}
