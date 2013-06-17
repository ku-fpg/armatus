package com.kufpg.armatus;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.kufpg.armatus.dialog.YesOrNoDialog;
import com.kufpg.armatus.util.FileIOUtils;

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

	public static String HISTORY_SOURCE_KEY, HISTORY_DIR_KEY, EDIT_MODE_KEY, RESTORE_DEFAULTS_KEY;
	private static final String CACHE_DIR = FileIOUtils.CACHE_DIR;
	private static PrefsActivity mActivity;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		getFragmentManager().beginTransaction().replace(android.R.id.content, new PrefsFragment()).commit();
		mActivity = this;
	}

	public static class PrefsFragment extends PreferenceFragment {
		private SharedPreferences mPrefs;
		private Editor mEditor;

		private static CheckBoxPreference mHistorySourcePref;
		private static EditTextPreference mHistoryDirPref;
		private static ListPreference mEditModePref;
		private static Preference mRestoreDefaultsPref;
		private static Map<Preference, String> mStaticPrefDefaults = new HashMap<Preference, String>();

		@Override
		public void onCreate(Bundle savedInstanceState) {
			super.onCreate(savedInstanceState);
			addPreferencesFromResource(R.xml.preferences);

			mPrefs = PreferenceManager.getDefaultSharedPreferences(mActivity);
			mEditor = mPrefs.edit();

			HISTORY_SOURCE_KEY = getResources().getString(R.string.pref_history_source);
			HISTORY_DIR_KEY = getResources().getString(R.string.pref_history_dir);
			EDIT_MODE_KEY = getResources().getString(R.string.pref_edit_mode);
			RESTORE_DEFAULTS_KEY = getResources().getString(R.string.pref_restore_defaults);

			mHistorySourcePref = (CheckBoxPreference) findPreference(HISTORY_SOURCE_KEY);
			mHistoryDirPref = (EditTextPreference) findPreference(HISTORY_DIR_KEY);
			mEditModePref = (ListPreference) findPreference(EDIT_MODE_KEY);
			mRestoreDefaultsPref = findPreference(RESTORE_DEFAULTS_KEY);

			mStaticPrefDefaults.put(mHistoryDirPref, CACHE_DIR);
			setStaticPrefValues(true).commit();
			updatePrefSummaries();

			mHistorySourcePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putBoolean("pref_history_source", (Boolean) newValue).commit();
					updatePrefSummaries();
					return true;
				}
			});

			mHistoryDirPref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putString("pref_history_dir", (String) newValue).commit();
					updatePrefSummaries();
					return true;
				}
			});

			mEditModePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putString("pref_editmode", (String) newValue).commit();
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

		private Editor restoreDefaultValues() {
			mEditor.clear();
			PreferenceManager.setDefaultValues(mActivity, R.xml.preferences, true);
			return setStaticPrefValues(false);
		}

		private Editor setStaticPrefValues(boolean initializing) {
			if (initializing) {
				for (Entry<Preference, String> entry : mStaticPrefDefaults.entrySet()) {
					if (entry.getKey() instanceof EditTextPreference) {
						EditTextPreference etp = (EditTextPreference) entry.getKey();
						if (mPrefs.getString(etp.getKey(), null) == null) {
							mEditor.putString(etp.getKey(), entry.getValue());
						}
					}
				}
			} else {
				for (Entry<Preference, String> entry : mStaticPrefDefaults.entrySet()) {
					if (entry.getKey() instanceof EditTextPreference) {
						EditTextPreference etp = (EditTextPreference) entry.getKey();
						mEditor.putString(etp.getKey(), entry.getValue());
					}
				}
			}
			return mEditor;
		}

		private void updatePrefSummaries() {
			if (mPrefs.getBoolean("pref_history_source", true)) {
				mHistoryDirPref.setSummary(mPrefs.getString("pref_history_dir", null));
			} else {
				mHistoryDirPref.setSummary(null);
			}
		}
	}
}
