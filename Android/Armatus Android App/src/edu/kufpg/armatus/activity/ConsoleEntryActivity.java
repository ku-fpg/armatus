package edu.kufpg.armatus.activity;

import android.os.Bundle;
import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.console.HermitClient;

public class ConsoleEntryActivity extends BaseActivity {
	
	private ConsoleEntry mEntry;
	private HermitClient mHermitClient;
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
		Bundle extras = getIntent().getExtras();
		if (extras != null) {
			mEntry = extras.getParcelable("entry");
			mHermitClient = extras.getParcelable("hermitClient");
		}
	}
	
	public ConsoleEntry getEntry() {
		return mEntry;
	}
	
	public HermitClient getHermitClient() {
		return mHermitClient;
	}

}
