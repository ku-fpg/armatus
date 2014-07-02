package edu.kufpg.armatus.networking;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothSocket;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import com.google.common.base.Optional;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

import java.io.IOException;
import java.util.UUID;

public class HermitBluetoothConnectRequest
        extends AsyncActivityTask<ConsoleActivity, Void, Void, Boolean> {
    private Optional<BluetoothAdapter> mAdapter;
    private Optional<BluetoothSocket> mSocket;
    private final int mUuidIndex;

    public HermitBluetoothConnectRequest(@NonNull final ConsoleActivity activity,
                                         final int uuidIndex) {
        super(activity);
        mUuidIndex = uuidIndex;
    }

    @Override protected void onPreExecute() {
        super.onPreExecute();

        getActivity().setProgressBarVisibility(true);
        getActivity().disableInput(true);
        mAdapter = BluetoothUtils.getBluetoothAdapter(getActivity());
        final UUID uuid = BluetoothUtils.UUIDS.get(mUuidIndex);
        mSocket = BluetoothUtils.getBluetoothSocket(getActivity(), uuid);
    }

    @NonNull
    @Override
    protected Boolean doInBackground(@Nullable final Void... params) {
        if (mAdapter.isPresent() && mSocket.isPresent()) {
            final BluetoothAdapter adapter = mAdapter.get();
            final BluetoothSocket socket = mSocket.get();

            if (socket.isConnected()) {
                return true;
            } else {
                adapter.cancelDiscovery();
                try {
                    socket.connect();
                    return true;
                } catch (final IOException ignored) {}
            }
        }

        return false;
    }

    @Override protected void onActivityAttached() {
        getActivity().getHermitClient().attachConnectRequest(mUuidIndex, this);
    }

    @Override protected void onCancelled(@NonNull final Boolean result) {
        super.onCancelled(result);
    }

    @Override protected void onPostExecute(@NonNull final Boolean result) {
        super.onPostExecute(result);

        if (result) {
            BluetoothUtils.setConnectedSocket(mSocket);
            getActivity().getHermitClient().bluetoothReceiveRequest(mUuidIndex);
        }
    }
}
