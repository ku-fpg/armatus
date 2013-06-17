package com.kufpg.armatus;

import android.app.ActionBar;
import android.app.ListActivity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.widget.Toast;

public class StandardListActivity extends ListActivity {

	public final static int FILE_FROM_DISK = 1;
	public static String PACKAGE_NAME;
	public static SharedPreferences mPrefs;
	public static Editor mEditor;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		ActionBar actionBar = getActionBar();
		actionBar.show();

		PACKAGE_NAME = getApplicationContext().getPackageName();
		mPrefs = PreferenceManager.getDefaultSharedPreferences(this);
		mEditor = mPrefs.edit();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.default_action_bar, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.menu_settings:
			Intent settingsActivity = new Intent(this, PrefsActivity.class);
			startActivity(settingsActivity);
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	public void showToast(String message) {
		Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
	}
	
	public void showToast(Object message) {
		showToast(message.toString());
	}
	
	public static SharedPreferences getPrefs() {
		return StandardActivity.getPrefs();
	}
	
	public static Editor getPrefsEditor() {
		return StandardListActivity.getPrefsEditor();
	}

}
