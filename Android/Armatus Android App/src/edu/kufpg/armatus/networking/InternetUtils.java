package edu.kufpg.armatus.networking;

import java.math.BigInteger;
import java.net.InetAddress;
import java.net.UnknownHostException;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.dialog.YesOrNoDialog;
import android.R.string;
import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.Fragment;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.wifi.WifiManager;
import android.os.Build;
import android.provider.Settings;

/**
 * Utility class containing methods that detail the network state of the device.
 */
public class InternetUtils {
	/** Request code for prompting the user to enable Wi-Fi. */
	public static final int REQUEST_ENABLE_WIFI = 31415;

	/** Monitors network connections. */
	private static ConnectivityManager sConMan;

	private static NetworkInfo sMobileInfo;

	/** Contains information about the current network connection (if there is one). */
	private static NetworkInfo sWifiInfo;
	
	private static String ip = null;

	private InternetUtils() {}

	/**
	 * Returns whether the device's airplane mode is on.
	 * @param context The {@link Context} to use.
	 * @return {@code true} if airplane mode is on.
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

	public static void enableWifi(final Activity activity) {
		String title = activity.getResources().getString(R.string.enable_wifi_title);
		String message = activity.getResources().getString(R.string.enable_wifi_message);
		YesOrNoDialog enableWifiDialog = new YesOrNoDialog(title, message) {
			@Override
			protected void yes(DialogInterface dialog, int whichButton) {
				Intent enableWifiIntent = new Intent(WifiManager.ACTION_PICK_WIFI_NETWORK);
				activity.startActivityForResult(enableWifiIntent, REQUEST_ENABLE_WIFI);
			}

			@Override
			protected void no(DialogInterface dialog, int whichButton) {
				super.no(dialog, whichButton);
				if (activity instanceof ConsoleActivity) {
					((ConsoleActivity) activity).onActivityResult(REQUEST_ENABLE_WIFI,
							Activity.RESULT_CANCELED, null);
				}
			}
		};
		enableWifiDialog.show(activity.getFragmentManager(), "enableWifi");
	}

	public static void enableWifi(final Fragment fragment) {
		String title = fragment.getResources().getString(R.string.enable_wifi_title);
		String message = fragment.getResources().getString(R.string.enable_wifi_message);
		YesOrNoDialog enableWifiDialog = new YesOrNoDialog(title, message) {
			@Override
			protected void yes(DialogInterface dialog, int whichButton) {
				Intent enableWifiIntent = new Intent(WifiManager.ACTION_PICK_WIFI_NETWORK);
				fragment.startActivityForResult(enableWifiIntent, REQUEST_ENABLE_WIFI);
			}

			@Override
			protected void no(DialogInterface dialog, int whichButton) {
				super.no(dialog, whichButton);
				if (fragment.getActivity() instanceof ConsoleActivity) {
					((ConsoleActivity) fragment.getActivity()).onActivityResult(REQUEST_ENABLE_WIFI,
							Activity.RESULT_CANCELED, null);
				}
			}
		};
		enableWifiDialog.show(fragment.getFragmentManager(), "enableWifi");
	}

	public static boolean isMobileConnected(Context context) {
		ConnectivityManager conMan = getConMan(context);		
		sMobileInfo = conMan.getNetworkInfo(ConnectivityManager.TYPE_MOBILE);
		if (sMobileInfo == null || !sMobileInfo.isConnected() || !sMobileInfo.isAvailable()) {
			return false;
		} else {
			return true;
		}
	}

	/**
	 * Returns whether the device's Wi-Fi is connected.
	 * @param context The {@link Context} to use.
	 * @return {@code true} if Wi-Fi is connected.
	 */
	public static boolean isWifiConnected(Context context) {
		ConnectivityManager conMan = getConMan(context);
		sWifiInfo = conMan.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
		if (sWifiInfo == null || !sWifiInfo.isConnected() || !sWifiInfo.isAvailable()) {
			return false;
		} else {
			return true;
		}
	}
	
	private static ConnectivityManager getConMan(Context context) {
		if (sConMan == null) {
			sConMan = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
		}
		return sConMan;
	}
	
	public static void DetectIP(Context context)
	{
		if(isWifiConnected(context) == true)
		{
		WifiManager wim = (WifiManager) context.getSystemService(Context.WIFI_SERVICE);
		int ipAddress = wim.getConnectionInfo().getIpAddress();
		byte[] ipAddressBytes = BigInteger.valueOf(ipAddress).toByteArray();
		InetAddress addr = null;
		try {
			addr = InetAddress.getByAddress(ipAddressBytes);
		} catch (UnknownHostException e) {
			e.printStackTrace();
		}
		ip = addr.getHostAddress();
		//showToast(addr.getHostAddress());
		}
	}
	
	public static String getIP(Context context)
	{
		if(ip == null)
		{
			DetectIP(context);
		}
		return ip;
	}

}
