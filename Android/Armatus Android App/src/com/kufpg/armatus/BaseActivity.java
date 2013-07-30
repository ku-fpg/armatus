package com.kufpg.armatus;

import java.util.Map;
import java.util.Map.Entry;

import com.google.common.collect.ImmutableMap;
import com.kufpg.armatus.EditManager.OnEditListener;

import android.app.ActionBar;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
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
	public static String HISTORY_USE_CACHE_KEY, HISTORY_DIR_KEY, EDIT_MODE_KEY, RESTORE_DEFAULTS_KEY, APP_THEME_KEY;
	public static String PACKAGE_NAME;
	private static Map<String, Object> STATIC_PREF_DEFAULTS_MAP;

	private static SharedPreferences mPrefs;
	private static SharedPreferences.Editor mEditor;
	private static EditManager mEditManager = new EditManager();
	private static MenuItem mUndoIcon, mRedoIcon;
	private static int mThemeId;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		PACKAGE_NAME = getApplicationContext().getPackageName();
		mPrefs = PreferenceManager.getDefaultSharedPreferences(this);
		mEditor = mPrefs.edit();

		HISTORY_USE_CACHE_KEY = getResources().getString(R.string.pref_history_use_cache);
		HISTORY_DIR_KEY = getResources().getString(R.string.pref_history_dir);
		EDIT_MODE_KEY = getResources().getString(R.string.pref_edit_mode);
		RESTORE_DEFAULTS_KEY = getResources().getString(R.string.pref_restore_defaults);
		APP_THEME_KEY = getResources().getString(R.string.pref_app_theme);
		STATIC_PREF_DEFAULTS_MAP = mapStaticPrefDefaults();

		PreferenceManager.setDefaultValues(this, R.xml.preferences, true);
		for (Entry<String, Object> entry : STATIC_PREF_DEFAULTS_MAP.entrySet()) {
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

		mThemeId = getThemePrefId();
		setTheme(mThemeId);

		super.onCreate(savedInstanceState);
		ActionBar actionBar = getActionBar();
		actionBar.show();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.console_list_view_menu, menu);
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
	protected void onResume() {
		if (mThemeId != getThemePrefId()) {
			recreate();
		}
		((BaseApplication<BaseActivity>) getApplication()).attach(this);
		super.onResume();
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		((BaseApplication<BaseActivity>) getApplication()).detach(this);
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

	protected EditManager getEditManager() {
		return mEditManager;
	}

	public static SharedPreferences.Editor getPrefsEditor() {
		return mEditor;
	}

	public static int getThemePrefId() {
		String theme = mPrefs.getString(APP_THEME_KEY, null);
		if (theme.equals(PrefsActivity.THEME_LIGHT)) {
			return R.style.ThemeLight;
		} else {
			return R.style.ThemeDark;
		}
	}

	public static void updateIconTitles() {
		mUndoIcon.setTitleCondensed(String.valueOf(mEditManager.getRemainingUndosCount()));
		mRedoIcon.setTitleCondensed(String.valueOf(mEditManager.getRemainingRedosCount()));
	}

	static Map<String, Object> getStaticPrefDefaults() {
		return STATIC_PREF_DEFAULTS_MAP;
	}

	private static Map<String, Object> mapStaticPrefDefaults() {
		ImmutableMap.Builder<String, Object> prefBuilder = ImmutableMap.builder();
		return prefBuilder.put(HISTORY_DIR_KEY, CACHE_DIR).build();
	}

	public enum EditMode {
		READ,
		WRITE,
		ARITHMETIC
	}

}
