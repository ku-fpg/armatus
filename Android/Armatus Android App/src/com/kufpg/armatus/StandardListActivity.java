package com.kufpg.armatus;

import android.app.ActionBar;
import android.app.ListActivity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.StrictMode;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.widget.Toast;

public class StandardListActivity extends ListActivity {

	public final static int FILE_FROM_DISK = 1;
	public static String PACKAGE_NAME;
	protected static String mSaveDir;
	protected static String mDefaultSaveDir;
	protected static String mEditModeValue;
	protected static SharedPreferences prefs;
	protected static SharedPreferences.Editor prefsEditor;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		ActionBar actionBar = getActionBar();
		actionBar.show();

		PACKAGE_NAME = getApplicationContext().getPackageName();
		mSaveDir = getCacheDir().toString();
		mDefaultSaveDir = mSaveDir;
		mEditModeValue = "0";
		prefs = PreferenceManager.getDefaultSharedPreferences(this);
		prefsEditor = prefs.edit();
		if (getSaveDir() == null) {
			loadPrefs();
		}

		// This prevents some exceptions from being thrown when the Internet is
		// accessed
		StrictMode.ThreadPolicy policy = new StrictMode.ThreadPolicy.Builder()
		.permitAll().build();
		StrictMode.setThreadPolicy(policy);
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
			Intent settingsActivity = new Intent(getBaseContext(),
					Preferences.class);
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

	public static String getSaveDir() {
		return StandardActivity.getSaveDir();
	}

	public static void setSaveDir(String saveDir) {
		StandardActivity.setSaveDir(saveDir);
	}

	public static String getDefaultSaveDir() {
		return StandardActivity.getDefaultSaveDir();
	}

	public static String getEditModeValue() {
		return StandardActivity.getEditModeValue();
	}

	public static void setEditModeValue(String editModeValue) {
		StandardActivity.setEditModeValue(editModeValue);
	}

	public static void setDefaultPrefs(Context context) {
		StandardActivity.setDefaultPrefs(context);
	}

	public static void loadPrefs() {
		StandardActivity.loadPrefs();
	}

}
