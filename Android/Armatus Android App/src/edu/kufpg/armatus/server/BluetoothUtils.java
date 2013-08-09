package edu.kufpg.armatus.server;

import java.util.UUID;

import edu.kufpg.armatus.BaseActivity;

import android.annotation.SuppressLint;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothManager;
import android.content.Context;
import android.content.SharedPreferences;
import android.os.Build;
import android.preference.PreferenceManager;

public class BluetoothUtils {
	public static final UUID BASE_UUID = UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");
	public static final int REQUEST_CONNECT_DEVICE = 777;
	public static final int REQUEST_ENABLE_BLUETOOTH = 8675309;
	
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
		return sAdapter;
	}
	
	public static BluetoothDevice getBluetoothDevice(Context context) {
		String address = getPrefs(context).getString(BaseActivity.BLUETOOTH_DEVICE_ADDRESS_KEY, null);
		if (address != null) {
			sDevice = getBluetoothAdapter(context).getRemoteDevice(address);
		}
		return sDevice;
	}
	
	public static void setBluetoothDevice(Context context, String friendlyName, String address) {
		getPrefsEditor(context).putString(BaseActivity.BLUETOOTH_DEVICE_NAME_KEY, friendlyName)
		.putString(BaseActivity.BLUETOOTH_DEVICE_ADDRESS_KEY, address)
		.commit();
		sDevice = getBluetoothAdapter(context).getRemoteDevice(address);
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