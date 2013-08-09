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

public class BluetoothUtils {
	public static final UUID BASE_UUID = UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");
	public static final int REQUEST_ENABLE_BLUETOOTH = 8675309;
	public static final int REQUEST_FIND_BLUETOOTH_DEVICE = 777;
	private static final String TAG = BluetoothUtils.class.getSimpleName();

	private static BluetoothAdapter sAdapter;
	private static BluetoothDevice sDevice;
	private static SharedPreferences sPrefs;
	private static SharedPreferences.Editor sPrefsEditor;

	private BluetoothUtils() {}

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

	public static BluetoothDevice getBluetoothDevice(Context context) {
		String address = getPrefs(context).getString(BaseActivity.BLUETOOTH_DEVICE_ADDRESS_KEY, null);
		if (address != null) {
			BluetoothAdapter adapter = getBluetoothAdapter(context);
			if (adapter != null) {
				sDevice = adapter.getRemoteDevice(address);
			} else {
				Log.w(TAG, "Cannot get Bluetooth device (Bluetooth not supported on this "
						+ BaseActivity.DEVICE_NAME + ").");
			}
		}
		return sDevice;
	}

	public static void enableBluetooth(Activity activity) {
		if (getBluetoothAdapter(activity) != null) {
			Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
			activity.startActivityForResult(enableBtIntent, REQUEST_ENABLE_BLUETOOTH);
		} else {
			Log.w(TAG, "Cannot enable Bluetooth (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}

	public static void enableBluetooth(Fragment fragment) {
		if (getBluetoothAdapter(fragment.getActivity()) != null) {
			Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
			fragment.startActivityForResult(enableBtIntent, REQUEST_ENABLE_BLUETOOTH);
		} else {
			Log.w(TAG, "Cannot enable Bluetooth (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}

	public static void findDeviceName(Activity activity) {
		if (getBluetoothAdapter(activity) != null) {
			Intent findBtDevicesIntent = new Intent(activity, BluetoothDeviceListActivity.class);
			activity.startActivityForResult(findBtDevicesIntent, REQUEST_FIND_BLUETOOTH_DEVICE);
		} else {
			Log.w(TAG, "Cannot find Bluetooth device names (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}
	
	public static void findDeviceName(Fragment fragment) {
		if (getBluetoothAdapter(fragment.getActivity()) != null) {
			Intent findBtDevicesIntent = new Intent(fragment.getActivity(), BluetoothDeviceListActivity.class);
			fragment.startActivityForResult(findBtDevicesIntent, REQUEST_FIND_BLUETOOTH_DEVICE);
		} else {
			Log.w(TAG, "Cannot find Bluetooth device names (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}

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

	public static void setBluetoothDevice(Context context, String friendlyName, String address) {
		getPrefsEditor(context).putString(BaseActivity.BLUETOOTH_DEVICE_NAME_KEY, friendlyName)
		.putString(BaseActivity.BLUETOOTH_DEVICE_ADDRESS_KEY, address)
		.commit();
		BluetoothAdapter adapter = getBluetoothAdapter(context);
		if (adapter != null) {
			sDevice = adapter.getRemoteDevice(address);
		} else {
			Log.w(TAG, "Cannot set Bluetooth device (Bluetooth not supported on this "
					+ BaseActivity.DEVICE_NAME + ").");
		}
	}

	private static SharedPreferences getPrefs(Context context) {
		if (sPrefs == null) {
			sPrefs = PreferenceManager.getDefaultSharedPreferences(context);
		}
		return sPrefs;
	}

	private static SharedPreferences.Editor getPrefsEditor(Context context) {
		if (sPrefsEditor == null) {
			sPrefsEditor = getPrefs(context).edit();
		}
		return sPrefsEditor;
	}

}