package com.kufpg.armatus.util;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.provider.Settings;

public class NetworkUtils {
	private static ConnectivityManager mConMan;
	private static NetworkInfo mNetworkInfo;

	private NetworkUtils() {}
	
	public static boolean isAirplaneModeOn(Context context) {
		return Settings.System.getInt(context.getContentResolver(),
				Settings.System.AIRPLANE_MODE_ON, 0) != 0;
	}

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
