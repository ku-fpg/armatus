package edu.kufpg.armatus.server;

import java.util.UUID;

import android.annotation.SuppressLint;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothManager;
import android.content.Context;
import android.os.Build;

public class BluetoothUtils {
	public static final UUID BASE_UUID = UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");
	public static final int REQUEST_CONNECT_DEVICE = 777;
	public static final int REQUEST_ENABLE_BLUETOOTH = 8675309;
	private static BluetoothAdapter mAdapter;

	private BluetoothUtils() {}

	@SuppressLint({ "InlinedApi", "NewApi" })
	public static BluetoothAdapter getBluetoothAdapter(Context context) {
		if (mAdapter == null) {
			if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR2) {
				mAdapter = BluetoothAdapter.getDefaultAdapter();
			} else {
				final BluetoothManager manager = (BluetoothManager) context.getSystemService(Context.BLUETOOTH_SERVICE);
				mAdapter = manager.getAdapter();
			}
		}
		return mAdapter;
	}

}