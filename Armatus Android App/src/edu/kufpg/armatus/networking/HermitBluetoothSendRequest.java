package edu.kufpg.armatus.networking;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothSocket;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import com.google.common.base.Optional;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

import java.io.IOException;
import java.io.OutputStream;

public class HermitBluetoothSendRequest
        extends AsyncActivityTask<ConsoleActivity, String, String, Boolean> {
    private Optional<BluetoothAdapter> mAdapter;
    //private Optional<BluetoothDevice> mDevice;
    private Optional<BluetoothSocket> mSocket;

    public HermitBluetoothSendRequest(@NonNull final ConsoleActivity activity) {
        super(activity);
    }

    @Override protected void onPreExecute() {
        super.onPreExecute();

        getActivity().setProgressBarVisibility(true);
        getActivity().disableInput(true);
        mAdapter = BluetoothUtils.getBluetoothAdapter(getActivity());
        //mDevice = BluetoothUtils.getBluetoothDevice(getActivity());
        mSocket = BluetoothUtils.getBluetoothSocket(getActivity());
    }

    @NonNull
    @Override
    protected Boolean doInBackground(@Nullable final String... params) {
        if (mSocket.isPresent() && mAdapter.isPresent()) {
            final BluetoothSocket socket = mSocket.get();
            final BluetoothAdapter adapter = mAdapter.get();

            if (!socket.isConnected() || BluetoothUtils.lastConnectionFailed()) {
                adapter.cancelDiscovery();
                try {
                    socket.connect();
                } catch (final IOException e) {
                    e.printStackTrace();
                    publishProgress("ERROR: Socket connection failed. Ensure that the server is up and try again.");
                    return false;
                }
            }

            publishProgress("Attempting to send data to server. Creating output stream...");
            final String message;
            if (params != null && params[0] != null) {
                message = params[0];
            } else {
                message = "No message provided!";
            }
            final byte[] messageBytes = message.getBytes();
            publishProgress("Output stream created! Sending message (" + message + ") to server...");
            final OutputStream outStream;

            try {
                outStream = socket.getOutputStream();
                outStream.write(messageBytes);
                outStream.flush();
            } catch (final IOException e) {
                e.printStackTrace();
                publishProgress("ERROR: Message sending failed. Ensure that the server is up and try again.");
                return false;
            }

            return true;
        }

        return false;
    }

    @Override protected void onCancelled() {
        end(true);
    }

    @Override protected void onPostExecute(@NonNull final Boolean result) {
        super.onPostExecute(result);
        if (result) {
            BluetoothUtils.notifyLastConnectionSucceeded();
        } else {
            BluetoothUtils.notifyLastConnectionFailed();
        }
        end(false);
    }

    private void end(final boolean cancelled) {
        if (getActivity().getHermitClient().isRequestDelayed()) {
            getActivity().getHermitClient().notifyDelayedRequestFinished();
        }

        if (cancelled) {
            getActivity().enableInput();
            getActivity().setProgressBarVisibility(false);
        }
    }
}
