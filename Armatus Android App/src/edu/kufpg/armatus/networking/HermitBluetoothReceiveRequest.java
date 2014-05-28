package edu.kufpg.armatus.networking;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

import java.io.IOException;
import java.io.InputStream;

/**
 * Created by xnux on 3/16/14.
 */
public class HermitBluetoothReceiveRequest extends AsyncActivityTask<ConsoleActivity, String, String, Void> {
    private BluetoothAdapter mAdapter;
    private BluetoothDevice mDevice;
    private BluetoothSocket mSocket;

    public HermitBluetoothReceiveRequest(ConsoleActivity activity) {
        super(activity);

//        getActivity().setProgressBarVisibility(true);
//        getActivity().disableInput(true);
        mAdapter = BluetoothUtils.getBluetoothAdapter(getActivity());
        mDevice = BluetoothUtils.getBluetoothDevice(getActivity());
        mSocket = BluetoothUtils.getBluetoothSocket(getActivity());
    }

    @Override
    protected void onPreExecute() {
        super.onPreExecute();

        getActivity().setProgressBarVisibility(true);
        getActivity().disableInput(true);
        mAdapter = BluetoothUtils.getBluetoothAdapter(getActivity());
        mDevice = BluetoothUtils.getBluetoothDevice(getActivity());
        mSocket = BluetoothUtils.getBluetoothSocket(getActivity());
    }

    @Override
    protected Void doInBackground(String... params) {
        if (mSocket != null && mDevice != null && mAdapter != null) {

            if (!mSocket.isConnected() || BluetoothUtils.lastConnectionFailed()) {
                mAdapter.cancelDiscovery();
                try {
                    mSocket.connect();
                } catch (IOException e) {
                    e.printStackTrace();
                    return null;
                }
            }

            publishProgress();

            InputStream inStream;
            try {
                inStream = mSocket.getInputStream();
            } catch (IOException e) {
                e.printStackTrace();
                return null;
            }

            byte[] buffer;
            int bytes;
            String response;
            while (true) {
                try {
                    /**
                     * WARNING! If the Android device is not connected to the server by this point,
                     * calling read() will crash the app without throwing an exception!
                     */
                    buffer = new byte[5092];
                    bytes = inStream.read(buffer);
                    response = new String(buffer, 0, bytes);
                    // Sometimes rogue NULL characters will come through, so filter them
                    if (response.codePointAt(0) != 0x0000 && response.length() > 1) {
                        publishProgress(response);
                    }
                } catch (IOException e) {
                    e.printStackTrace();
                    return null;
                }
            }
        }

        return null;
    }

    @Override
    protected void onPostExecute(Void result) {
        super.onPostExecute(result);
        showInput();
    }

    @Override
    protected void onProgressUpdate(String... progress) {
        if (progress.length != 0) {
            getActivity().getHermitClient().handleInput(progress[0]);
        }
        showInput();
    }

    private void showInput() {
        getActivity().enableInput();
        getActivity().setProgressBarVisibility(false);
    }
}
