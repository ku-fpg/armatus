package com.kufpg.armatus;

import java.io.File;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import com.ipaulpro.afilechooser.FileChooserActivity;
import com.ipaulpro.afilechooser.utils.FileUtils;
import com.kufpg.armatus.dialog.YesOrNoDialog;

import android.app.Activity;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
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

public class PrefsActivity extends PreferenceActivity {

	private static final String HISTORY_SOURCE_KEY = BaseActivity.HISTORY_SOURCE_KEY;
	private static final String HISTORY_DIR_KEY = BaseActivity.HISTORY_DIR_KEY;
	private static final String EDIT_MODE_KEY = BaseActivity.EDIT_MODE_KEY;
	private static final String RESTORE_DEFAULTS_KEY = BaseActivity.RESTORE_DEFAULTS_KEY;
	private static final int DIR_CHANGE_CODE = 777;
	
	private static Activity mActivity;
	private static SharedPreferences mPrefs;
	private static Editor mEditor;
	private static CheckBoxPreference mHistorySourcePref;
	private static ListPreference mEditModePref;
	private static Preference mRestoreDefaultsPref, mHistoryDirPref;
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
			mHistoryDirPref = findPreference(HISTORY_DIR_KEY);
			mEditModePref = (ListPreference) findPreference(EDIT_MODE_KEY);
			mRestoreDefaultsPref = findPreference(RESTORE_DEFAULTS_KEY);

			mStaticPrefDefaults = BaseActivity.getStaticPrefDefaults();
			updatePrefSummaries();

			mHistorySourcePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					mEditor.putBoolean(HISTORY_SOURCE_KEY, (Boolean) newValue).commit();
					updatePrefSummaries();
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
						updatePrefSummaries();
					}
				}
				break;
			}
			super.onActivityResult(requestCode, resultCode, data);
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
		
		private void showToast(String message) {
			Toast.makeText(mActivity, message, Toast.LENGTH_SHORT).show();
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
