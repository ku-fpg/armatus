package edu.kufpg.armatus.server;

import java.util.UUID;

import edu.kufpg.armatus.BaseActivity;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.Fragment;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothManager;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.preference.PreferenceManager;
import android.util.Log;

/**
 * Utility class that organizes all of the crazy Android Bluetooth API calls into much
 * more intuitive method names.
 */
public class BluetoothUtils {
	/** 
	 * A wrapper around a 128-bit number used to identify this service. This UUID is the Bluetooth
	 * Base UUID and is commonly used for simple Bluetooth applications. Regardless of the UUID used,
	 * it must match the one that the Armatus Bluetooth server is advertising.
	 * */
	public static final UUID BASE_UUID = UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");

	/** Request code for prompting the user to enable Bluetooth. */
	public static final int REQUEST_ENABLE_BLUETOOTH = 8675309;

	/** Request code for prompting the user to select a Bluetooth device from a list of nearby
	 * devices. */
	public static final int REQUEST_FIND_BLUETOOTH_DEVICE = 777;

	/** Identifies this class for {@link Log} purposes. */
	private static final String TAG = BluetoothUtils.class.getSimpleName();

	/** Reference to the device's Bluetooth adapter. */
	private static BluetoothAdapter sAdapter;

	/** Reference to the app's {@link SharedPreferences}. */
	private static SharedPreferences sPrefs;

	/** Reference to the {@code Editor} that can change the app's {@link SharedPreferences}. */
	private static SharedPreferences.Editor sPrefsEditor;

	private BluetoothUtils() {}

	/**
	 * Retrieves the device's Bluetooth adapter (if it supports Bluetooth).
	 * @param context The {@link Context} to use.
	 * @return the default Bluetooth adapter, or {@code null} if the device does not support
	 * Bluetooth.
	 */
	@SuppressLint({ "InlinedApi", "NewApi" })
	public static BluetoothAdapter getBluetoothAdapter(Context context) {
		if (sAdapter == null) {
			if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR2) {
				sAdapter = BluetoothAdapter.getDefaultAdapter();
			} else {
				final BluetoothManager manager = (BluetoothManager) context.getSystemService(Context.BLUETOOTH_SERVICE);
				sAdapter = manager.getAdapter();
			}
		}
		if (sAdapter == null) {
			Log.w(TAG, "Bluetooth is not supported on this " + BaseActivity.DEVICE_NAME + "!");
		}
		return sAdapter;
	}

	/**
	 * Retrieves the {@code BluetoothDevice} as specified by the app's {@link SharedPreferences}.
	 * Make sure that {@link #setBluetoothDeviceInfo(Context, String, String)} has been called
	 * before using this method.
	 * @param context The {@link Context} to use.
	 * @return The {@code BluetoothDevice} corresponding to user preferences. {@code null} is
	 * returned if no {@code BluetoothDevice} is found in user preferences or if Bluetooth is
	 * not supported on the device.
	 */
	public static BluetoothDevice getBluetoothDevice(Context context) {
		String address = getPrefs(context).getString(BaseActivity.BLUETOOTH_DEVICE_ADDRESS_KEY, null);
		BluetoothDevice device = null;
		if (address != null) {
			BluetoothAdapter adapter = getBluetoothAdapter(context);
			if (adapter != null) {
				device = adapter.getRemoteDevice(address);
			} else {
				Log.w(TAG, "Cannot get Bluetooth device (Bluetooth not supported on this "
						+ BaseActivity.DEVICE_NAME + ").");
			}
		}
		return device;
	}

	/**
	 * If Bluetooth is not already on, this method prompts the user to turn on Bluetooth.
	 * @param activity The {@link Activity} that will handle the result (with the {@code
	 * requestCode} {@link #REQUEST_ENABLE_BLUETOOTH}).
	 */
	public static void enableBluetooth(Activity activity) {
		if (getBluetoothAdapter(activity) != null) {
			Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
			activity.startActivityForResult(enableBtIntent, REQUEST_ENABLE_BLUETOOTH);
		} else {
			Log.w(TAG, "Cannot enable Bluetooth (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}

	/**
	 * If Bluetooth is not already on, this method prompts the user to turn on Bluetooth.
	 * @param activity The {@link Fragment} that will handle the result (with the {@code
	 * requestCode} {@link #REQUEST_ENABLE_BLUETOOTH}).
	 */
	public static void enableBluetooth(Fragment fragment) {
		if (getBluetoothAdapter(fragment.getActivity()) != null) {
			Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
			fragment.startActivityForResult(enableBtIntent, REQUEST_ENABLE_BLUETOOTH);
		} else {
			Log.w(TAG, "Cannot enable Bluetooth (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}

	/**
	 * Starts an {@link Intent} that allows the user to select a Bluetooth device from a list
	 * of nearby devices.
	 * @param activity The {@link Activity} that will handle the result (with the {@code
	 * requestCode} {@link #REQUEST_FIND_BLUETOOTH_DEVICE}).
	 */
	public static void findDeviceName(Activity activity) {
		if (getBluetoothAdapter(activity) != null) {
			Intent findBtDevicesIntent = new Intent(activity, BluetoothDeviceListActivity.class);
			activity.startActivityForResult(findBtDevicesIntent, REQUEST_FIND_BLUETOOTH_DEVICE);
		} else {
			Log.w(TAG, "Cannot find Bluetooth device names (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}

	/**
	 * Starts an {@link Intent} that allows the user to select a Bluetooth device from a list
	 * of nearby devices.
	 * @param activity The {@link Fragment} that will handle the result (with the {@code
	 * requestCode} {@link #REQUEST_FIND_BLUETOOTH_DEVICE}).
	 */
	public static void findDeviceName(Fragment fragment) {
		if (getBluetoothAdapter(fragment.getActivity()) != null) {
			Intent findBtDevicesIntent = new Intent(fragment.getActivity(), BluetoothDeviceListActivity.class);
			fragment.startActivityForResult(findBtDevicesIntent, REQUEST_FIND_BLUETOOTH_DEVICE);
		} else {
			Log.w(TAG, "Cannot find Bluetooth device names (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}

	/**
	 * Returns whether Bluetooth is turned on.
	 * @param context The {@link Context} to use.
	 * @return {@code true} if Bluetooth is on, or {@code false} if Bluetooth is off or not
	 * supported on the device.
	 */
	public static boolean isBluetoothEnabled(Context context) {
		BluetoothAdapter adapter = getBluetoothAdapter(context);
		if (adapter != null) {
			return adapter.isEnabled();
		} else {
			Log.w(TAG, "Cannot determine Bluetooth state (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
			return false;
		}
	}

	/**
	 * Configures the user preferences for the name and MAC address of the Bluetooth device
	 * to look for when connecting to an Armatus Bluetooth server.
	 * @param context The {@link Context} to use.
	 * @param friendlyName The name that identifies the Bluetooth device.
	 * @param address The MAC address of the Bluetooth device. It should be in this format:
	 * "XX:XX:XX:XX:XX" (where an X should be replaced by any hexadecimal number).
	 */
	public static void setBluetoothDeviceInfo(Context context, String friendlyName, String address) {
		getPrefsEditor(context).putString(BaseActivity.BLUETOOTH_DEVICE_NAME_KEY, friendlyName)
		.putString(BaseActivity.BLUETOOTH_DEVICE_ADDRESS_KEY, address)
		.commit();
	}

	/**
	 * Convenience for retrieving the app's default {@link SharedPreferences}.
	 * @param context The {@link Context} to use.
	 * @return The app's {@code SharedPreferences}.
	 */
	private static SharedPreferences getPrefs(Context context) {
		if (sPrefs == null) {
			sPrefs = PreferenceManager.getDefaultSharedPreferences(context);
		}
		return sPrefs;
	}

	/**
	 * Convenience for retrieving the {@code Editor} that can change the app's
	 * {@link SharedPreferences}.
	 * @param context The {@link Context} to use.
	 * @return The app's {@code SharedPreferences.Editor}.
	 */
	private static SharedPreferences.Editor getPrefsEditor(Context context) {
		if (sPrefsEditor == null) {
			sPrefsEditor = getPrefs(context).edit();
		}
		return sPrefsEditor;
	}

}