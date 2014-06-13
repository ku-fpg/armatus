package edu.kufpg.armatus.networking;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

import java.io.IOException;
import java.io.OutputStream;

public class HermitBluetoothSendRequest extends AsyncActivityTask<ConsoleActivity, String, String, Boolean> {
    private BluetoothAdapter mAdapter;
    private BluetoothDevice mDevice;
    private BluetoothSocket mSocket;

    public HermitBluetoothSendRequest(ConsoleActivity activity) {
        super(activity);
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
    protected Boolean doInBackground(String... params) {
        if (mSocket != null && mDevice != null && mAdapter != null) {

            if (!mSocket.isConnected() || BluetoothUtils.lastConnectionFailed()) {
                mAdapter.cancelDiscovery();
                try {
                    mSocket.connect();
                } catch (IOException e) {
                    e.printStackTrace();
                    publishProgress("ERROR: Socket connection failed. Ensure that the server is up and try again.");
                    return false;
                }
            }

            publishProgress("Attempting to send data to server. Creating output stream...");
            String message = params[0];
            if (message == null) {
                message = "No message provided!";
            }
            byte[] messageBytes = message.getBytes();
            publishProgress("Output stream created! Sending message (" + message + ") to server...");
            OutputStream outStream;
            try {
                outStream = mSocket.getOutputStream();
                outStream.write(messageBytes);
                outStream.flush();
            } catch (IOException e) {
                e.printStackTrace();
                publishProgress("ERROR: Message sending failed. Ensure that the server is up and try again.");
                return false;
            }

            return true;
        }

        return false;
    }

//    @Override
//    protected void onProgressUpdate(String... progress) {
//        getActivity().getHermitClient().handleInput(progress[0]);
//    }

    @Override
    protected void onCancelled() {
        end(true);
    }

    @Override
    protected void onPostExecute(Boolean result) {
        super.onPostExecute(result);
        if (result != null) {
            BluetoothUtils.notifyLastConnectionSucceeded();
        } else {
            BluetoothUtils.notifyLastConnectionFailed();
        }
        end(false);
    }

    private void end(boolean cancelled) {
        if (getActivity().getHermitClient().isRequestDelayed()) {
            getActivity().getHermitClient().notifyDelayedRequestFinished();
        }

        if (cancelled) {
            getActivity().enableInput();
            getActivity().setProgressBarVisibility(false);
        }
    }
}
