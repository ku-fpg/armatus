package com.kufpg.armatus;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.os.Bundle;
import android.preference.EditTextPreference;
import android.preference.ListPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceActivity;
import android.preference.PreferenceFragment;

public class Preferences extends PreferenceActivity {

	private static Context mContext;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		getFragmentManager().beginTransaction().replace(android.R.id.content, new PrefsFragment()).commit();
		mContext = this;
	}

	public static class PrefsFragment extends PreferenceFragment {
		private static EditTextPreference saveDirPref;
		private static ListPreference editModePref;
		private static Preference defaultPref;

		@Override
		public void onCreate(Bundle savedInstanceState) {
			super.onCreate(savedInstanceState);
			addPreferencesFromResource(R.xml.preferences);

			saveDirPref = (EditTextPreference) findPreference("savedir_pref");
			// saveDirPref's default value is not static, so its value has to be
			// "initialized" in a special fashion
			saveDirPref.setText(StandardActivity.getSaveDir());
			editModePref = (ListPreference) findPreference("editmode_pref");
			defaultPref = findPreference("default_pref_restore");

			saveDirPref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					StandardActivity.setSaveDir((String) newValue);
					StandardActivity.loadPrefs();
					return true;
				}
			});

			editModePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					StandardActivity.setEditModeValue((String) newValue);
					StandardActivity.loadPrefs();
					return false;
				}
			});

			defaultPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					AlertDialog.Builder defaultPrefsAlert = new AlertDialog.Builder(mContext);
					defaultPrefsAlert.setMessage(R.string.default_pref_message);
					defaultPrefsAlert.setCancelable(true);
					defaultPrefsAlert.setPositiveButton("Yes",
							new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int whichButton) {
							// saveDirPref's default value is not static, so its value has to be
							//"initialized" in a special fashion
							saveDirPref.setText(StandardActivity.getDefaultSaveDir());
							StandardActivity.setDefaultPrefs(mContext);
						}
					});
					defaultPrefsAlert.setNegativeButton("No", new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog, int whichButton) {}
					});

					defaultPrefsAlert.show();
					return true;
				}
			});
		}
	}
}
