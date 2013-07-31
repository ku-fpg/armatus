package com.kufpg.armatus;

import java.io.File;
import java.util.Map;
import java.util.Map.Entry;

import com.ipaulpro.afilechooser.FileChooserActivity;
import com.ipaulpro.afilechooser.utils.FileUtils;
import com.kufpg.armatus.dialog.YesOrNoDialog;

import android.app.Activity;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.OnSharedPreferenceChangeListener;
import android.net.Uri;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.ListPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceActivity;
import android.preference.PreferenceFragment;
import android.widget.Toast;

/**
 * The {@link Activity} that displays user preferences (via {@link PrefsFragment}).
 */
public class PrefsActivity extends PreferenceActivity {

	/**
	 * One of the possible values that the {@link Preference} to which {@link #APP_THEME_KEY}
	 * maps can be (the other being {@link #THEME_LIGHT}).
	 */
	public static final String THEME_DARK = "THEME_DARK";

	/**
	 * One of the possible values that the {@link Preference} to which {@link #APP_THEME_KEY}
	 * maps can be (the other being {@link #THEME_DARK}).
	 */
	public static final String THEME_LIGHT = "THEME_LIGHT";

	/**
	 * {@link CheckBoxPreference} key mapping to whether or not {@link BaseActivity#CACHE_DIR
	 * CACHE_DIR} should be used to save persistent data. If not, the String to which
	 * {@link #HISTORY_DIR_KEY} maps is used instead.
	 */
	private static final String HISTORY_USE_CACHE_KEY = BaseActivity.HISTORY_USE_CACHE_KEY;

	/**
	 * {@link Preference} key mapping to the String representation of a directory where persistent
	 * data can be stored. The directory is only used if the value to which {@link
	 * #HISTORY_USE_CACHE_KEY} maps is true.
	 */
	private static final String HISTORY_DIR_KEY = BaseActivity.HISTORY_DIR_KEY;

	/**
	 * {@link ListPreference} key mapping to one of three String values: "0" (for {@link
	 * BaseActivity.EditMode#READ READ} mode), "1" (for {@link BaseActivity.EditMode#WRITE WRITE}
	 * mode), or "2" (for {@link BaseActivity.EditMode#ARITHMETIC ARITHMETIC} mode). The mapped
	 * String represent which {@link BaseActivity.EditMode EditMode} is currently being used.
	 */
	private static final String EDIT_MODE_KEY = BaseActivity.EDIT_MODE_KEY;

	/**
	 * {@link ListPreference} key mapping to either {@link #THEME_DARK} or {@link #THEME_LIGHT},
	 * depending on which theme is currently being used.
	 */
	private static final String APP_THEME_KEY = BaseActivity.APP_THEME_KEY;

	/**
	 * {@link Preference} key used for resetting preferences back to their default values.
	 */
	private static final String RESTORE_DEFAULTS_KEY = BaseActivity.RESTORE_DEFAULTS_KEY;

	/** Request code used for selecting a directory with aFileChooser. */
	private static final int DIR_CHANGE_CODE = 777;

	/**
	 * Maps special {@link Preference} keys to their default values when the default values are
	 * impossible to know before runtime (e.g., the external cache directory, which {@link
	 * #HISTORY_USE_CACHE_KEY} maps to by default).
	 */
	private static Map<String, ? extends Object> DYNAMIC_PREF_DEFAULTS_MAP;

	/**
	 * A reference to {@link PrefsActivity} that is used for methods that require {@link
	 * android.content.Context Context} in {@link PrefsFragment}.
	 */
	private static Activity mActivity;
	
	/** Used to access persistent user preferences. Editing them requires {@link #mEditor}. */
	private static SharedPreferences mPrefs;
	
	/** Used to edit persistent user preferences stored in {@link #mPrefs}. */
	private static SharedPreferences.Editor mEditor;
	
	/**
	 * 
	 */
	private static CheckBoxPreference mHistoryUseCachePref;
	private static ListPreference mEditModePref, mAppThemePref;
	private static Preference mRestoreDefaultsPref, mHistoryDirPref;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		setTheme(BaseActivity.getThemePrefId());
		getFragmentManager().beginTransaction().replace(android.R.id.content, new PrefsFragment()).commit();
		mActivity = this;

		super.onCreate(savedInstanceState);
	}

	public static class PrefsFragment extends PreferenceFragment {
		@Override
		public void onCreate(Bundle savedInstanceState) {
			super.onCreate(savedInstanceState);
			addPreferencesFromResource(R.xml.preferences);

			mPrefs = PreferenceManager.getDefaultSharedPreferences(mActivity);
			mPrefs.registerOnSharedPreferenceChangeListener(new OnSharedPreferenceChangeListener() {
				@Override
				public void onSharedPreferenceChanged(SharedPreferences sharedPreferences, String key) {
					updatePrefSummary(key);
				}
			});
			mEditor = mPrefs.edit();

			mHistoryUseCachePref = (CheckBoxPreference) findPreference(HISTORY_USE_CACHE_KEY);
			mHistoryDirPref = findPreference(HISTORY_DIR_KEY);
			mEditModePref = (ListPreference) findPreference(EDIT_MODE_KEY);
			mAppThemePref = (ListPreference) findPreference(APP_THEME_KEY);
			mRestoreDefaultsPref = findPreference(RESTORE_DEFAULTS_KEY);

			DYNAMIC_PREF_DEFAULTS_MAP = BaseActivity.getDyanmicPrefDefaults();
			for (Entry<String, ?> entry : mPrefs.getAll().entrySet()) {
				updatePrefSummary(entry.getKey());
			}

			mHistoryUseCachePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putBoolean(HISTORY_USE_CACHE_KEY, (Boolean) newValue).commit();
					return true;
				}
			});

			mHistoryDirPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					Intent intent = new Intent(mActivity, FileChooserActivity.class);
					intent.setType(FileUtils.MIME_TYPE_TEXT);
					intent.addCategory(Intent.CATEGORY_OPENABLE);
					startActivityForResult(intent, DIR_CHANGE_CODE);
					return true;
				}
			});

			mEditModePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putString(EDIT_MODE_KEY, (String) newValue).commit();
					return true;
				}
			});

			mAppThemePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putString(APP_THEME_KEY, (String) newValue).commit();
					mActivity.recreate();
					return true;
				}
			});

			mRestoreDefaultsPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					String message = getResources().getString(R.string.default_pref_message);
					YesOrNoDialog restorePrefsDialog = new YesOrNoDialog("Restore default preferences", message) {
						@Override
						protected void yes(DialogInterface dialog, int whichButton) {
							restoreDefaultValues().commit();
							mActivity.finish();
							Intent settingsActivity = new Intent(mActivity, PrefsActivity.class);
							startActivity(settingsActivity);
						}
					};

					restorePrefsDialog.show(getFragmentManager(), "restoreDefaultPrefs");
					return true;
				}
			});
		}

		@Override
		public void onActivityResult(int requestCode, int resultCode, Intent data) {
			switch (requestCode) {
			case DIR_CHANGE_CODE:
				if (resultCode == RESULT_OK) {
					if (data != null) {
						final Uri uri = data.getData();
						final File file = FileUtils.getFile(uri);
						String dir = file.getAbsolutePath();
						if (file.isFile()) {
							dir = file.getParent();
						} else if (!file.exists()) {
							showToast("Error: directory does not exist"); //Should never happen
							break;
						}
						mEditor.putString(HISTORY_DIR_KEY, dir).commit();
					}
				}
				break;
			}
			super.onActivityResult(requestCode, resultCode, data);
		}

		private static SharedPreferences.Editor restoreDefaultValues() {
			mEditor.clear();
			PreferenceManager.setDefaultValues(mActivity, R.xml.preferences, true);
			return setDyanmicPrefValues();
		}

		private static SharedPreferences.Editor setDyanmicPrefValues() {
			for (Entry<String, ? extends Object> entry : DYNAMIC_PREF_DEFAULTS_MAP.entrySet()) {
				if (entry.getValue() instanceof String) {
					mEditor.putString(entry.getKey(), (String) entry.getValue());
				}
			}
			return mEditor;
		}

		private void showToast(String message) {
			Toast.makeText(mActivity, message, Toast.LENGTH_SHORT).show();
		}

		private static void updatePrefSummary(String key) {
			if (key.equals(HISTORY_USE_CACHE_KEY)) {
				if (mPrefs.getBoolean(HISTORY_USE_CACHE_KEY, true)) {
					mHistoryDirPref.setSummary(mPrefs.getString(HISTORY_DIR_KEY, null));
				} else {
					mHistoryDirPref.setSummary(null);
				}
			} else if (key.equals(APP_THEME_KEY)) {
				if (mPrefs.getString(APP_THEME_KEY, null).equals(THEME_DARK)) {
					mAppThemePref.setSummary("Dark theme");
				} else if (mPrefs.getString(APP_THEME_KEY, null).equals(THEME_LIGHT)) {
					mAppThemePref.setSummary("Light theme");
				}
			}
		}
	}
}
