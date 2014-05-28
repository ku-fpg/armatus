package edu.kufpg.armatus.activity;

import android.os.Bundle;
import android.view.MenuItem;
import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.console.HermitClient;

public class ConsoleEntryActivity extends BaseActivity {

	private ConsoleEntry mEntry;
	private HermitClient mHermitClient;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		getActionBar().setDisplayHomeAsUpEnabled(true);

		Bundle extras = getIntent().getExtras();
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

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case android.R.id.home:
			finish();
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

}
