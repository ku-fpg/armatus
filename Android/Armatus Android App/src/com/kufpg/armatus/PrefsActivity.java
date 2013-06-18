package com.kufpg.armatus;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.kufpg.armatus.dialog.YesOrNoDialog;

import android.app.Activity;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.EditTextPreference;
import android.preference.ListPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceActivity;
import android.preference.PreferenceFragment;

public class PrefsActivity extends PreferenceActivity {

	public static String HISTORY_SOURCE_KEY = StandardActivity.HISTORY_SOURCE_KEY;
	public static String HISTORY_DIR_KEY = StandardActivity.HISTORY_DIR_KEY;
	public static String EDIT_MODE_KEY = StandardActivity.EDIT_MODE_KEY;
	public static String RESTORE_DEFAULTS_KEY = StandardActivity.RESTORE_DEFAULTS_KEY;
	private static Activity mActivity;
	private static SharedPreferences mPrefs;
	private static Editor mEditor;
	private static CheckBoxPreference mHistorySourcePref;
	private static EditTextPreference mHistoryDirPref;
	private static ListPreference mEditModePref;
	private static Preference mRestoreDefaultsPref;
	private static Map<String, Object> mStaticPrefDefaults = new HashMap<String, Object>();

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		getFragmentManager().beginTransaction().replace(android.R.id.content, new PrefsFragment()).commit();
		mActivity = this;
	}

	public static class PrefsFragment extends PreferenceFragment {
		@Override
		public void onCreate(Bundle savedInstanceState) {
			super.onCreate(savedInstanceState);
			addPreferencesFromResource(R.xml.preferences);

			mPrefs = PreferenceManager.getDefaultSharedPreferences(mActivity);
			mEditor = mPrefs.edit();

			mHistorySourcePref = (CheckBoxPreference) findPreference(HISTORY_SOURCE_KEY);
			mHistoryDirPref = (EditTextPreference) findPreference(HISTORY_DIR_KEY);
			mEditModePref = (ListPreference) findPreference(EDIT_MODE_KEY);
			mRestoreDefaultsPref = findPreference(RESTORE_DEFAULTS_KEY);

			mStaticPrefDefaults = StandardActivity.getStaticPrefDefaults();
			updatePrefSummaries();

			mHistorySourcePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putBoolean(HISTORY_SOURCE_KEY, (Boolean) newValue).commit();
					updatePrefSummaries();
					return true;
				}
			});

			mHistoryDirPref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putString(HISTORY_DIR_KEY, (String) newValue).commit();
					updatePrefSummaries();
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

		private static Editor restoreDefaultValues() {
			mEditor.clear();
			PreferenceManager.setDefaultValues(mActivity, R.xml.preferences, true);
			return setStaticPrefValues();
		}

		private static Editor setStaticPrefValues() {
			for (Entry<String, Object> entry : mStaticPrefDefaults.entrySet()) {
				if (entry.getValue() instanceof String) {
					mEditor.putString(entry.getKey(), (String) entry.getValue());
				}
			}
			return mEditor;
		}

		private static void updatePrefSummaries() {
			if (mPrefs.getBoolean(HISTORY_SOURCE_KEY, true)) {
				mHistoryDirPref.setSummary(mPrefs.getString(HISTORY_DIR_KEY, null));
			} else {
				mHistoryDirPref.setSummary(null);
			}
		}
	}
}
