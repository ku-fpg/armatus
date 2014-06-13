package edu.kufpg.armatus.networking;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.app.Fragment;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothManager;
import android.bluetooth.BluetoothSocket;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Build;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.Log;
import com.google.common.collect.ImmutableList;
import edu.kufpg.armatus.DeviceConstants;
import edu.kufpg.armatus.Prefs;

import java.io.IOException;
import java.util.UUID;

/**
 * Utility class that organizes all of the crazy Android Bluetooth API calls into much
 * more intuitive method names.
 */
public final class BluetoothUtils {
    /**
     * A wrapper around a 128-bit number used to identify this service. This UUID is the Bluetooth
     * Base UUID and is commonly used for simple Bluetooth applications. Regardless of the UUID used,
     * it must match the one that the Armatus Bluetooth server is advertising.
     */
    public static final UUID BASE_UUID = UUID.fromString("00001101-0000-1000-8000-00805F9B34FB");
    public static final UUID UUID1 = UUID.fromString("b7746a40-c758-4868-aa19-7ac6b3475dfc");
    public static final UUID UUID2 = UUID.fromString("2d64189d-5a2c-4511-a074-77f199fd0834");
    public static final UUID UUID3 = UUID.fromString("e442e09a-51f3-4a7b-91cb-f638491d1412");
    public static final UUID UUID4 = UUID.fromString("a81d6504-4536-49ee-a475-7d96d09439e4");
    public static final UUID UUID5 = UUID.fromString("aa91eab1-d8ad-448e-abdb-95ebba4a9b55");
    public static final UUID UUID6 = UUID.fromString("4d34da73-d0a4-4f40-ac38-917e0a9dee97");
    public static final UUID UUID7 = UUID.fromString("5e14d4df-9c8a-4db7-81e4-c937564c86e0");
    public static final ImmutableList<UUID> UUIDS = ImmutableList.of(UUID1, UUID2, UUID3, UUID4, UUID5, UUID6, UUID7);

    /**
     * Request code for prompting the user to enable Bluetooth.
     */
    public static final int REQUEST_ENABLE_BLUETOOTH = 8675309;

    /**
     * Request code for prompting the user to select a Bluetooth device from a list of nearby
     * devices.
     */
    public static final int REQUEST_FIND_BLUETOOTH_DEVICE = 777;

    /**
     * Identifies this class for {@link Log} purposes.
     */
    private static final String TAG = BluetoothUtils.class.getSimpleName();

    /**
     * Reference to the device's Bluetooth adapter.
     */
    @Nullable private static BluetoothAdapter sAdapter;
    @Nullable private static BluetoothDevice sDevice;
    @Nullable private static BluetoothSocket sSocket;

    private static boolean sLastConnectionFailed = false;

    private BluetoothUtils() {}

    public static void closeBluetooth() {
        if (sSocket != null) {
            try {
//				if (sSocket.getOutputStream() != null) {
//					sSocket.getOutputStream().close();
//				}
//				if (sSocket.getInputStream() != null) {
//					sSocket.getInputStream().close();
//				}
                sSocket.close();
            } catch (final IOException e) {
                Log.w(TAG, "Error occurred when closing Bluetooth socket!");
                e.printStackTrace();
            }
            sSocket = null;
        }
        sDevice = null;
    }

    /**
     * If Bluetooth is not already on, this method prompts the user to turn on Bluetooth.
     *
     * @param activity The {@link Activity} that will handle the result (with the {@code
     *                 requestCode} {@link #REQUEST_ENABLE_BLUETOOTH}).
     */
    public static void enableBluetooth(@NonNull final Activity activity) {
        if (getBluetoothAdapter(activity) != null) {
            final Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
            activity.startActivityForResult(enableBtIntent, REQUEST_ENABLE_BLUETOOTH);
        } else {
            Log.w(TAG, "Cannot enable Bluetooth (Bluetooth not supported on this "
                    + DeviceConstants.DEVICE_NAME + ").");
        }
    }

    /**
     * If Bluetooth is not already on, this method prompts the user to turn on Bluetooth.
     *
     * @param fragment The {@link Fragment} that will handle the result (with the {@code
     *                 requestCode} {@link #REQUEST_ENABLE_BLUETOOTH}).
     */
    public static void enableBluetooth(@NonNull final Fragment fragment) {
        if (getBluetoothAdapter(fragment.getActivity()) != null) {
            final Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
            fragment.startActivityForResult(enableBtIntent, REQUEST_ENABLE_BLUETOOTH);
        } else {
            Log.w(TAG, "Cannot enable Bluetooth (Bluetooth not supported on this "
                    + DeviceConstants.DEVICE_NAME + ").");
        }
    }

    /**
     * Retrieves the device's Bluetooth adapter (if it supports Bluetooth).
     *
     * @param context The {@link Context} to use.
     * @return the default Bluetooth adapter, or {@code null} if the device does not support
     * Bluetooth.
     */
    @SuppressLint({"InlinedApi", "NewApi"})
    @Nullable public static BluetoothAdapter getBluetoothAdapter(@NonNull final Context context) {
        if (sAdapter == null) {
            if (Build.VERSION.SDK_INT < Build.VERSION_CODES.JELLY_BEAN_MR2) {
                sAdapter = BluetoothAdapter.getDefaultAdapter();
            } else {
                final BluetoothManager manager = (BluetoothManager) context.getSystemService(Context.BLUETOOTH_SERVICE);
                sAdapter = manager.getAdapter();
            }
        }
        if (sAdapter == null) {
            Log.w(TAG, "Bluetooth is not supported on this " + DeviceConstants.DEVICE_NAME + '!');
        }
        return sAdapter;
    }

    /**
     * Retrieves the {@code BluetoothDevice} as specified by the app's {@link SharedPreferences}.
     *
     * @param context The {@link Context} to use.
     * @return The {@code BluetoothDevice} corresponding to user preferences. {@code null} is
     * returned if no {@code BluetoothDevice} is found in user preferences or if Bluetooth is
     * not supported on the device.
     */
    @Nullable public static BluetoothDevice getBluetoothDevice(@NonNull final Context context) {
        final String address = Prefs.getBluetoothDeviceAddress(context);
        if (address != null) {
            final BluetoothAdapter adapter = getBluetoothAdapter(context);
            if (adapter != null) {
                if (sDevice == null) {
                    sDevice = adapter.getRemoteDevice(address);
                }
                return sDevice;
            } else {
                Log.w(TAG, "Cannot get Bluetooth device (Bluetooth not supported on this "
                        + DeviceConstants.DEVICE_NAME + ").");
                return null;
            }
        } else {
            Log.w(TAG, "Cannot get Bluetooth device (no device address found in preferences).");
            return null;
        }
    }

    @Nullable public static BluetoothSocket getBluetoothSocket(@NonNull final Context context) {
        final BluetoothAdapter adapter = getBluetoothAdapter(context);
        if (adapter != null) {
            final BluetoothDevice device = getBluetoothDevice(context);
            if (device != null) {
                if (sSocket == null) {
                    try {
                        sSocket = device.createRfcommSocketToServiceRecord(BASE_UUID);
                    } catch (final IOException e) {
                        e.printStackTrace();
                        Log.w(TAG, "Cannot get Bluetooth socket (creation failed).");
                        return null;
                    }
                }
                return sSocket;
            } else {
                Log.w(TAG, "Cannot get Bluetooth socket (no device address found in preferences).");
                return null;
            }
        } else {
            Log.w(TAG, "Cannot get Bluetooth socket (Bluetooth not supported on this "
                    + DeviceConstants.DEVICE_NAME + ").");
            return null;
        }
    }

    /**
     * Starts an {@link Intent} that allows the user to select a Bluetooth device from a list
     * of nearby devices.
     *
     * @param activity The {@link Activity} that will handle the result (with the {@code
     *                 requestCode} {@link #REQUEST_FIND_BLUETOOTH_DEVICE}).
     */
    public static void findDeviceName(@NonNull final Activity activity) {
        if (getBluetoothAdapter(activity) != null) {
            final Intent findBtDevicesIntent = new Intent(activity, BluetoothDeviceListActivity.class);
            activity.startActivityForResult(findBtDevicesIntent, REQUEST_FIND_BLUETOOTH_DEVICE);
        } else {
            Log.w(TAG, "Cannot find Bluetooth device names (Bluetooth not supported on this "
                    + DeviceConstants.DEVICE_NAME + ").");
        }
    }

    /**
     * Starts an {@link Intent} that allows the user to select a Bluetooth device from a list
     * of nearby devices.
     *
     * @param fragment The {@link Fragment} that will handle the result (with the {@code
     *                 requestCode} {@link #REQUEST_FIND_BLUETOOTH_DEVICE}).
     */
    public static void findDeviceName(@NonNull final Fragment fragment) {
        if (getBluetoothAdapter(fragment.getActivity()) != null) {
            final Intent findBtDevicesIntent = new Intent(fragment.getActivity(), BluetoothDeviceListActivity.class);
            fragment.startActivityForResult(findBtDevicesIntent, REQUEST_FIND_BLUETOOTH_DEVICE);
        } else {
            Log.w(TAG, "Cannot find Bluetooth device names (Bluetooth not supported on this "
                    + DeviceConstants.DEVICE_NAME + ").");
        }
    }

    public static boolean isBluetoothConnected(@NonNull final Context context) {
        final BluetoothAdapter adapter = getBluetoothAdapter(context);
        if (adapter != null) {
            final BluetoothDevice device = getBluetoothDevice(context);
            if (device != null) {
                final BluetoothSocket socket = getBluetoothSocket(context);
                if (socket != null) {
                    return socket.isConnected();
                } else {
                    Log.w(TAG, "Cannot determine Bluetooth state (socket creation failed).");
                    return false;
                }
            } else {
                Log.w(TAG, "Cannot determine Bluetooth state (no device found in preferences).");
                return false;
            }
        } else {
            Log.w(TAG, "Cannot determine Bluetooth state (Bluetooth not supported on this "
                    + DeviceConstants.DEVICE_NAME + ").");
            return false;
        }
    }

    /**
     * Returns whether Bluetooth is turned on.
     *
     * @param context The {@link Context} to use.
     * @return {@code true} if Bluetooth is on, or {@code false} if Bluetooth is off or not
     * supported on the device.
     */
    public static boolean isBluetoothEnabled(@NonNull final Context context) {
        final BluetoothAdapter adapter = getBluetoothAdapter(context);
        if (adapter != null) {
            return adapter.isEnabled();
        } else {
            Log.w(TAG, "Cannot determine Bluetooth state (Bluetooth not supported on this "
                    + DeviceConstants.DEVICE_NAME + ").");
            return false;
        }
    }

    public static boolean lastConnectionFailed() {
        return sLastConnectionFailed;
    }

    public static void notifyLastConnectionFailed() {
        sLastConnectionFailed = true;
    }

    public static void notifyLastConnectionSucceeded() {
        sLastConnectionFailed = false;
    }

}