package com.kufpg.armatus;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.kufpg.armatus.console.EditManager;
import com.kufpg.armatus.console.EditManager.OnEditListener;

import android.app.ActionBar;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.Environment;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.widget.Toast;

public class BaseActivity extends Activity {

	public static final String CACHE_DIR = Environment.getExternalStorageDirectory().getPath() + "/data/armatus";
	public static String HISTORY_SOURCE_KEY, HISTORY_DIR_KEY, EDIT_MODE_KEY, RESTORE_DEFAULTS_KEY;
	public static String PACKAGE_NAME;
	private static Map<String, Object> mStaticPrefDefaults = new HashMap<String, Object>();
	public static SharedPreferences mPrefs;
	public static Editor mEditor;
	public static EditManager mEditManager = new EditManager();
	public static MenuItem mUndoIcon, mRedoIcon;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		ActionBar actionBar = getActionBar();
		actionBar.show();

		PACKAGE_NAME = getApplicationContext().getPackageName();
		mPrefs = PreferenceManager.getDefaultSharedPreferences(this);
		mEditor = mPrefs.edit();
		HISTORY_SOURCE_KEY = getResources().getString(R.string.pref_history_source);
		HISTORY_DIR_KEY = getResources().getString(R.string.pref_history_dir);
		EDIT_MODE_KEY = getResources().getString(R.string.pref_edit_mode);
		RESTORE_DEFAULTS_KEY = getResources().getString(R.string.pref_restore_defaults);

		mStaticPrefDefaults.put(HISTORY_DIR_KEY, CACHE_DIR);

		PreferenceManager.setDefaultValues(this, R.xml.preferences, true);
		for (Entry<String, Object> entry : mStaticPrefDefaults.entrySet()) {
			if (entry.getValue() instanceof String) {
				if (mPrefs.getString(entry.getKey(), null) == null) {
					mEditor.putString(entry.getKey(), (String) entry.getValue());
				}
			}
		}
		mEditor.commit();
		mEditManager.setOnEditListener(new OnEditListener() {
			@Override
			public void onEditFinish() {
				updateIconTitles();
			}
		});
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.default_action_bar, menu);
		mUndoIcon = menu.findItem(R.id.undo);
		mRedoIcon = menu.findItem(R.id.redo);
		updateIconTitles();
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.undo:
			if (canUndo()) {
				mEditManager.undo();
				updateIconTitles();
			}
			return true;
		case R.id.redo:
			if (canRedo()) {
				mEditManager.redo();
				updateIconTitles();
			}
			return true;
		case R.id.menu_settings:
			Intent settingsActivity = new Intent(this, PrefsActivity.class);
			startActivity(settingsActivity);
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);

		((BaseApplication<BaseActivity>) getApplication()).detach(this);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onRestoreInstanceState(Bundle savedInstanceState) {
		super.onRestoreInstanceState(savedInstanceState);

		((BaseApplication<BaseActivity>) getApplication()).attach(this);
	}
	
	public boolean canRedo() {
		return mEditManager.canRedo();
	}
	
	public boolean canUndo() {
		return mEditManager.canUndo();
	}

	public void showToast(String message) {
		Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
	}

	public void showToast(Object message) {
		showToast(message.toString());
	}

	public static boolean appInstalledOrNot(Context context, String uri) {
		PackageManager pm = context.getPackageManager();
		boolean appInstalled = false;
		try {
			pm.getPackageInfo(uri, PackageManager.GET_ACTIVITIES);
			appInstalled = true;
		} catch (PackageManager.NameNotFoundException e){
			appInstalled = false;
		}
		return appInstalled;
	}

	public static SharedPreferences getPrefs() {
		return mPrefs;
	}

	public EditManager getEditManager() {
		return mEditManager;
	}

	public static Editor getPrefsEditor() {
		return mEditor;
	}

	public static void updateIconTitles() {
		mUndoIcon.setTitleCondensed(String.valueOf(mEditManager.getRemainingUndosCount()));
		mRedoIcon.setTitleCondensed(String.valueOf(mEditManager.getRemainingRedosCount()));
	}

	static Map<String, Object> getStaticPrefDefaults() {
		return mStaticPrefDefaults;
	}

	public enum EditMode {
		READ,
		WRITE,
		ARITHMETIC
	}

}
