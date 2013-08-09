/*
 * Copyright (C) 2009 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package edu.kufpg.armatus.server;

import java.util.Set;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.R;

import android.app.Activity;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.AdapterView.OnItemClickListener;

/**
 * Lists any paired Bluetooth devices and devices detected in the area after initiating a
 * Bluetooth discovery. When a device is chosen by the user, the MAC address of the device
 * is sent back to the parent {@link Activity} in the result {@link Intent}.
 */
public class BluetoothDeviceListActivity extends BaseActivity {
	/** Used as a string extra value in the resulting {@link Intent}. Contains the chosen
	 * Bluetooth device's friendly name. */
	public static final String EXTRA_DEVICE_NAME = "device_name";
	
	/** Used as a string extra value in the resulting {@link Intent}. Contains the chosen
	 * Bluetooth device's MAC address. */
	public static final String EXTRA_DEVICE_ADDRESS = "device_address";
	
	/** Identifies this class for debugging purposes. */
	private static final String TAG = "DeviceListActivity";
	
	/** Toggles whether Logcat debug messages are enabled. */
	private static final boolean DEBUG = false;

	/** Can discover Bluetooth devices, cancel device discovery, and list bonded devices. */
	private BluetoothAdapter mBluetoothAdapter;
	
	/** Manages list data for bonded Bluetooth devices. */
	private ArrayAdapter<BluetoothDevice> mPairedDevicesArrayAdapter;
	
	/** Manages list data for unbonded Bluetooth devices. */
	private ArrayAdapter<BluetoothDevice> mNewDevicesArrayAdapter;
	
	/** Enables or disables a Bluetooth device scan. */
	private Button mScanButton;
	
	/** Tracks if there is an ongoing Bluetooth device scan. */
	private boolean mScanInProgress = false;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
		super.onCreate(savedInstanceState);
		setContentView(R.layout.bluetooth_device_list);

		// Set result RESULT_CANCELED in case the user backs out
		setResult(Activity.RESULT_CANCELED);

		mScanButton = (Button) findViewById(R.id.bluetooth_scan_button);
		mScanButton.setOnClickListener(new OnClickListener() {
			public void onClick(View v) {
				if (mScanInProgress) {
					stopDiscovery();
				} else {
					doDiscovery();
				}
			}
		});

		mPairedDevicesArrayAdapter = new BluetoothDeviceListAdapter(this);
		mNewDevicesArrayAdapter = new BluetoothDeviceListAdapter(this);

		ListView pairedListView = (ListView) findViewById(R.id.bluetooth_paired_devices_list);
		pairedListView.setAdapter(mPairedDevicesArrayAdapter);
		pairedListView.setOnItemClickListener(mDeviceClickListener);

		ListView newDevicesListView = (ListView) findViewById(R.id.bluetooth_new_devices_list);
		newDevicesListView.setAdapter(mNewDevicesArrayAdapter);
		newDevicesListView.setOnItemClickListener(mDeviceClickListener);

		IntentFilter filter = new IntentFilter(BluetoothDevice.ACTION_FOUND);
		registerReceiver(mReceiver, filter);

		filter = new IntentFilter(BluetoothAdapter.ACTION_DISCOVERY_FINISHED);
		registerReceiver(mReceiver, filter);

		mBluetoothAdapter = BluetoothUtils.getBluetoothAdapter(this);
		Set<BluetoothDevice> pairedDevices = mBluetoothAdapter.getBondedDevices();

		if (pairedDevices.size() > 0) {
			findViewById(R.id.bluetooth_paired_devices_title).setVisibility(View.VISIBLE);
			for (BluetoothDevice device : pairedDevices) {
				mPairedDevicesArrayAdapter.add(device);
			}
		} else {
			mPairedDevicesArrayAdapter.add(null);
		}
	}

	/** Disable the options menu of {@link BaseActivity}. */
	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		return false;
	}

	@Override
	protected void onDestroy() {
		super.onDestroy();

		if (mBluetoothAdapter != null) {
			mBluetoothAdapter.cancelDiscovery();
		}

		unregisterReceiver(mReceiver);
	}

	/** Starts device discovery with {@link #mBluetoothAdapter}. */
	private void doDiscovery() {
		if (DEBUG) Log.d(TAG, "doDiscovery()");

		setProgressBarIndeterminateVisibility(true);
		setTitle(R.string.bluetooth_scanning_title);
		findViewById(R.id.bluetooth_new_devices_title).setVisibility(View.VISIBLE);
		mScanButton.setText(R.string.bluetooth_button_cancel);

		if (mBluetoothAdapter.isDiscovering()) {
			mBluetoothAdapter.cancelDiscovery();
		}

		mBluetoothAdapter.startDiscovery();
		mScanInProgress = true;
	}

	/** Ends device discovery. */
	private void stopDiscovery() {
		if (DEBUG) Log.d(TAG, "stopDiscovery()");

		setProgressBarIndeterminateVisibility(false);
		setTitle(R.string.bluetooth_select_device_title);
		mScanButton.setText(R.string.bluetooth_button_scan);

		if (mBluetoothAdapter.isDiscovering()) {
			mBluetoothAdapter.cancelDiscovery();
		}

		mScanInProgress = false;
	}

	/** Handles clicks for all {@link ListView} items in the {@link Activity}. */
	private OnItemClickListener mDeviceClickListener = new OnItemClickListener() {
		public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
			// Cancel discovery because it's costly and we're about to connect
			mBluetoothAdapter.cancelDiscovery();

			// Get the device MAC address
			BluetoothDevice device = (BluetoothDevice) parent.getItemAtPosition(position);
			if (device != null) { //Make sure "No devices" is not clicked
				String name = device.getName();
				String address = device.getAddress();

				// Create the result Intent and include the MAC address
				Intent intent = new Intent();
				intent.putExtra(EXTRA_DEVICE_NAME, name);
				intent.putExtra(EXTRA_DEVICE_ADDRESS, address);

				// Set result and finish this Activity
				setResult(Activity.RESULT_OK, intent);
				finish();
			}
		}
	};

	/** Listens for discovered devices and changes the title when discovery is finished. */
	private final BroadcastReceiver mReceiver = new BroadcastReceiver() {
		@Override
		public void onReceive(Context context, Intent intent) {
			String action = intent.getAction();

			if (BluetoothDevice.ACTION_FOUND.equals(action)) {
				BluetoothDevice device = intent.getParcelableExtra(BluetoothDevice.EXTRA_DEVICE);
				// If it's already paired, skip it, because it's been listed already
				if (device.getBondState() != BluetoothDevice.BOND_BONDED) {
					mNewDevicesArrayAdapter.add(device);
				}
			} else if (BluetoothAdapter.ACTION_DISCOVERY_FINISHED.equals(action)) {
				setProgressBarIndeterminateVisibility(false);
				setTitle(R.string.bluetooth_select_device_title);
				mScanButton.setText(R.string.bluetooth_button_scan);
				mScanInProgress = false;
				if (mNewDevicesArrayAdapter.getCount() == 0) {
					mNewDevicesArrayAdapter.add(null);
				}
			}
		}
	};

	/** Populates Bluetooth device information (i.e., its name and MAC address). */
	private static class BluetoothDeviceListAdapter extends ArrayAdapter<BluetoothDevice> {
		private LayoutInflater mInflater;

		/**
		 * Constructs a new instance with the specified {@link Context}.
		 * @param context The {@code Context} to use.
		 */
		public BluetoothDeviceListAdapter(Context context) {
			super(context, R.layout.bluetooth_device_list_entry);
			mInflater = LayoutInflater.from(context);
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			final BluetoothDeviceHolder holder;
			if (convertView == null) {
				convertView = mInflater.inflate(R.layout.bluetooth_device_list_entry, parent, false);
				holder = new BluetoothDeviceHolder();
				holder.name = (TextView) convertView.findViewById(R.id.bluetooth_device_name);
				holder.address = (TextView) convertView.findViewById(R.id.bluetooth_device_address);
				convertView.setTag(holder);
			} else {
				holder = (BluetoothDeviceHolder) convertView.getTag();
			}

			BluetoothDevice device = getItem(position);

			if (device != null) {
				holder.name.setText(device.getName());
				holder.address.setText(device.getAddress());
			} else {
				holder.name.setText("No devices");
				holder.address.setText(null);
			}

			return convertView;
		}

		/** Holds the entries' {@link TextView}s for efficiency purposes. */
		private static class BluetoothDeviceHolder {
			private TextView name, address;
		}

	}

}
