package edu.kufpg.armatus.activity;

import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.view.MenuItem;
import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.console.HermitClient;

public class ConsoleEntryActivity extends BaseActivity {

    private ConsoleEntry mEntry;
    private HermitClient mHermitClient;

    @Override public void onCreate(@Nullable final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getActionBar() != null) {
            getActionBar().setDisplayHomeAsUpEnabled(true);
        }

        final Bundle extras = getIntent().getExtras();
        if (extras != null) {
            mEntry = extras.getParcelable(ConsoleEntryIntent.ENTRY_TAG);
            mHermitClient = extras.getParcelable(ConsoleEntryIntent.CLIENT_TAG);
        }
    }

    public ConsoleEntry getEntry() {
        return mEntry;
    }

    public HermitClient getHermitClient() {
        return mHermitClient;
    }

    @Override public boolean onOptionsItemSelected(@NonNull final MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                finish();
                return true;
            default:
                return super.onOptionsItemSelected(item);
        }
    }

}
