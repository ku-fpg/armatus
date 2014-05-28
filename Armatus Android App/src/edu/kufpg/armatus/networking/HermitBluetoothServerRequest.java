package edu.kufpg.armatus.networking;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public class HermitBluetoothServerRequest extends AsyncActivityTask<ConsoleActivity, String, String, String> {
    private BluetoothAdapter mAdapter;
    private BluetoothDevice mDevice;
    private BluetoothSocket mSocket;

    public HermitBluetoothServerRequest(ConsoleActivity console) {
        super(console);
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
    protected String doInBackground(String... params) {
        if (mSocket != null && mDevice != null && mAdapter != null) {

            if (!mSocket.isConnected() || BluetoothUtils.lastConnectionFailed()) {
                mAdapter.cancelDiscovery();
                try {
                    mSocket.connect();
                } catch (IOException e) {
                    e.printStackTrace();
                    publishProgress("ERROR: Socket connection failed. Ensure that the server is up and try again.");
                    return null;
                }
            }

            publishProgress("Attempting to send data to server. Creating output stream...");
            String message = params[0];
            if (message == null) {
                message = "No message provided!";
            }
            byte[] messageBytes = message.getBytes();
            publishProgress("Output stream created! Sending message (" + message + ") to server...");
            OutputStream outStream = null;
            try {
                outStream = mSocket.getOutputStream();
                outStream.write(messageBytes);
                outStream.flush();
            } catch (IOException e) {
                e.printStackTrace();
                publishProgress("ERROR: Message sending failed. Ensure that the server is up and try again.");
                return null;
            }

            publishProgress("Message sent! Preparing for server response...");
            InputStream inStream;
            try {
                inStream = mSocket.getInputStream();
            } catch (IOException e) {
                e.printStackTrace();
                publishProgress("ERROR: Input stream creation failed. Ensure that the server is up and try again.");
                return null;
            }
            String response;
            try {
                /**
                 * WARNING! If the Android device is not connected to the server by this point,
                 * calling read() will crash the app without throwing an exception!
                 */
                byte[] buffer = new byte[5092];
                int bytes = inStream.read(buffer);
                response = new String(buffer, 0, bytes);
            } catch (IOException e) {
                e.printStackTrace();
                publishProgress("ERROR: Failed to read server response. Ensure that the server is up and try again.");
                return null;
            }

            return response;
        }

        return null;
    }

//	@Override
//	protected void onProgressUpdate(String... progress) {
//		//getActivity().appendErrorResponse(progress[0]);
//	}

    @Override
    protected void onCancelled() {
        end();
    }

    @Override
    protected void onPostExecute(String result) {
        super.onPostExecute(result);
        if (result != null) {
            getActivity().appendErrorResponse(result);
            BluetoothUtils.notifyLastConnectionSucceeded();
        } else {
            BluetoothUtils.notifyLastConnectionFailed();
        }
        end();
    }

    private void end() {
        if (getActivity().getHermitClient().isRequestDelayed()) {
            getActivity().getHermitClient().notifyDelayedRequestFinished();
        }

        getActivity().enableInput();
        getActivity().setProgressBarVisibility(false);
    }

}