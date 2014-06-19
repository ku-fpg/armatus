package edu.kufpg.armatus.networking;

import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothSocket;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import com.google.common.base.Function;
import com.google.common.base.Optional;
import com.google.common.collect.Lists;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

import java.io.IOException;
import java.io.InputStream;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

public class HermitBluetoothReceiveRequest
        extends AsyncActivityTask<ConsoleActivity, String, String, Void> {
    private Optional<BluetoothAdapter> mAdapter;
    private final List<Optional<BluetoothSocket>> mSockets;

    public HermitBluetoothReceiveRequest(@NonNull final ConsoleActivity activity) {
        super(activity);
        mSockets = Lists.transform(Arrays.asList(0, 1, 2, 3, 4, 5, 6),
                new Function<Integer, Optional<BluetoothSocket>>() {
                    @Override public Optional<BluetoothSocket> apply(final Integer i) {
                        final UUID uuid = BluetoothUtils.UUIDS.get(i);
                        return BluetoothUtils.getBluetoothSocket(activity, uuid);
                    }
                });
    }

    @Override protected void onPreExecute() {
        super.onPreExecute();

        if (getActivity() != null) {
            getActivity().setProgressBarVisibility(true);
            getActivity().disableInput(true);
            mAdapter = BluetoothUtils.getBluetoothAdapter(getActivity());
        }
    }

    @Override protected Void doInBackground(@Nullable final String... params) {
        if (mAdapter.isPresent()) {
            final BluetoothAdapter adapter = mAdapter.get();
            adapter.cancelDiscovery();

            final BluetoothSocket connSocket;

            for (final Optional<BluetoothSocket> maybeSocket : mSockets) {
                if (maybeSocket.isPresent()) {
                    final BluetoothSocket socket = maybeSocket.get();
                    if (!socket.isConnected() || BluetoothUtils.lastConnectionFailed()) {
                        try {
                            socket.connect();
                            connSocket = socket;
                            break;
                        } catch (final IOException e) {
                            e.printStackTrace();
                            return null;
                        }
                    }
                } else {
                    return null;
                }
            }


            publishProgress();

            final InputStream inStream;
            try {
                inStream = connSocket.getInputStream();
            } catch (final IOException e) {
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
                } catch (final IOException e) {
                    e.printStackTrace();
                    return null;
                }
            }
        }

        return null;
    }

    @Override protected void onPostExecute(@Nullable final Void result) {
        super.onPostExecute(result);
        showInput();
    }

    @Override protected void onProgressUpdate(@Nullable final String... progress) {
        if (progress != null && progress.length != 0 && progress[0] != null) {
            getActivity().getHermitClient().handleInput(progress[0]);
        }
        showInput();
    }

    private void showInput() {
        getActivity().enableInput();
        getActivity().setProgressBarVisibility(false);
    }
}
