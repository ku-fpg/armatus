package edu.kufpg.armatus.networking;

import android.bluetooth.BluetoothAdapter;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import com.google.common.base.Optional;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

public class HermitBluetoothConnectRequest
        extends AsyncActivityTask<ConsoleActivity, Integer, Void, Void> {
    private Optional<BluetoothAdapter> mAdapter;

    public HermitBluetoothConnectRequest(@NonNull final ConsoleActivity activity) {
        super(activity);
    }

    @Override protected void onPreExecute() {
        super.onPreExecute();
        if (getActivity() != null) {
            mAdapter = BluetoothUtils.getBluetoothAdapter(getActivity());
        }
    }

    @Override protected Void doInBackground(@Nullable final Integer... params) {
        if (mAdapter.isPresent()) {
            final BluetoothAdapter adapter = mAdapter.get();
            adapter.cancelDiscovery();
        }




        return null;
    }

    @Override protected void onCancelled(@Nullable final Void result) {
        super.onCancelled(result);
    }

    @Override protected void onPostExecute(@Nullable final Void result) {
        super.onPostExecute(result);
    }
}
