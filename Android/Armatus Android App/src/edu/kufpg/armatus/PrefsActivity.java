package edu.kufpg.armatus;

import java.io.File;
import java.util.Map;
import java.util.Map.Entry;

import com.ipaulpro.afilechooser.FileChooserActivity;
import com.ipaulpro.afilechooser.utils.FileUtils;

import edu.kufpg.armatus.dialog.BluetoothDeviceListActivity;
import edu.kufpg.armatus.dialog.YesOrNoDialog;
import edu.kufpg.armatus.server.BluetoothUtils;

import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
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

	/** Request code used for selecting a directory with aFileChooser. */
	private static final int DIR_CHANGE_CODE = 4242;

	/**
	 * Maps special {@link Preference} keys to their default values when the default values are
	 * impossible to know before runtime (e.g., the external cache directory, which {@link
	 * BaseActivity#HISTORY_USE_CACHE_KEY HISTORY_USE_CACHE_KEY} maps to by default).
	 */
	private static Map<String, ? extends Object> DYNAMIC_PREF_DEFAULTS_MAP;

	/** Used to access persistent user preferences. Editing them requires {@link #sEditor}. */
	private static SharedPreferences sPrefs;

	/** Used to edit persistent user preferences stored in {@link #sPrefs}. */
	private static SharedPreferences.Editor sEditor;

	/**
	 * Preference that adjusts whether or not {@link BaseActivity#CACHE_DIR CACHE_DIR}
	 * should be used to save persistent data (the value mapped to by {@link
	 * BaseActivity#HISTORY_USE_CACHE_KEY HISTORY_USE_CACHE_KEY}).
	 */
	private static CheckBoxPreference sHistoryUseCachePref;

	/**
	 * Preference that can change the String representation of a directory where persistent
	 * data can be stored (the value mapped to by {@link BaseActivity#HISTORY_DIR_KEY
	 * HISTORY_DIR_KEY}).
	 */
	private static Preference sHistoryDirPref;

	/**
	 * Preference that can change the string representation of what the current {@link
	 * BaseActivity.EditMode EditMode} is (the value mapped to by {@link BaseActivity#EDIT_MODE_KEY
	 * EDIT_MODE_KEY}).
	 */
	private static ListPreference sEditModePref;

	/**
	 * Preference that can change the current app theme (the value mapped to by {@link
	 * BaseActivity#APP_THEME_KEY APP_THEME_KEY}).
	 */
	private static ListPreference sAppThemePref;

	/**
	 * Preference that can change the current network source (the value mapped to by
	 * {@link BaseActivity#NETWORK_SOURCE_KEY NETWORK_SOURCE_KEY}).
	 */
	private static ListPreference sNetworkSourcePref;
	
	/**
	 * Preference that can change the Bluetooth device used if Bluetooth communications
	 * are enabled (the value mapped to by {@link BaseActivity#CHOOSE_BLUETOOTH_DEVICE_KEY
	 * CHOOSE_BLUETOOTH_DEVICE_KEY}).
	 */
	private static Preference sChooseBluetoothDevicePref;

	/**
	 * Preference that can restore the default app preferences (mapped to by {@link
	 * BaseActivity#RESTORE_DEFAULTS_KEY RESTORE_DEFAULTS_KEY}).
	 */
	private static Preference sRestoreDefaultsPref;
	
	/**
	 * {@link Preference} key used to choose the Bluetooth device if Bluetooth communications
	 * are enabled.
	 */
	private static String CHOOSE_BLUETOOTH_DEVICE_KEY;

	/**
	 * {@link Preference} key used for resetting preferences back to their default values.
	 */
	private static String RESTORE_DEFAULTS_KEY;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		setTheme(BaseActivity.getThemePrefId());
		getFragmentManager().beginTransaction().replace(android.R.id.content, new PrefsFragment()).commit();
		CHOOSE_BLUETOOTH_DEVICE_KEY = getResources().getString(R.string.pref_choose_bluetooth_device);
		RESTORE_DEFAULTS_KEY = getResources().getString(R.string.pref_restore_defaults);
		
		super.onCreate(savedInstanceState);
	}

	/**
	 * Where the user can change the app preferences.
	 */
	public static class PrefsFragment extends PreferenceFragment {
		
		@Override
		public void onCreate(Bundle savedInstanceState) {
			super.onCreate(savedInstanceState);
			addPreferencesFromResource(R.xml.preferences);

			sPrefs = BaseActivity.getPrefs();
			sEditor = BaseActivity.getPrefsEditor();

			sHistoryUseCachePref = (CheckBoxPreference) findPreference(BaseActivity.HISTORY_USE_CACHE_KEY);
			sHistoryDirPref = findPreference(BaseActivity.HISTORY_DIR_KEY);
			sEditModePref = (ListPreference) findPreference(BaseActivity.EDIT_MODE_KEY);
			sAppThemePref = (ListPreference) findPreference(BaseActivity.APP_THEME_KEY);
			sNetworkSourcePref = (ListPreference) findPreference(BaseActivity.NETWORK_SOURCE_KEY);
			sChooseBluetoothDevicePref = (Preference) findPreference(CHOOSE_BLUETOOTH_DEVICE_KEY);
			sRestoreDefaultsPref = findPreference(RESTORE_DEFAULTS_KEY);

			DYNAMIC_PREF_DEFAULTS_MAP = BaseActivity.getDyanmicPrefDefaults();

			sHistoryUseCachePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					if ((Boolean) newValue) {
						sHistoryDirPref.setSummary(sPrefs.getString(BaseActivity.HISTORY_DIR_KEY, null));
					} else {
						sHistoryDirPref.setSummary(null);
					}
					return true;
				}
			});

			sHistoryDirPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					Intent intent = new Intent(getActivity(), FileChooserActivity.class);
					intent.setType(FileUtils.MIME_TYPE_TEXT);
					intent.addCategory(Intent.CATEGORY_OPENABLE);
					startActivityForResult(intent, DIR_CHANGE_CODE);
					return true;
				}
			});

			sEditModePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					//TODO: Change edit mode
					return true;
				}
			});

			sAppThemePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					getActivity().recreate();
					return true;
				}
			});
			
			sNetworkSourcePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					//TODO: Change network source
					return true;
				}
			});
			
			sChooseBluetoothDevicePref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					Intent serverIntent = new Intent(getActivity(), BluetoothDeviceListActivity.class);
					startActivityForResult(serverIntent, BluetoothUtils.REQUEST_CONNECT_DEVICE);
					return true;
				}
			});

			sRestoreDefaultsPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					String message = getResources().getString(R.string.default_pref_message);
					YesOrNoDialog restorePrefsDialog = new YesOrNoDialog("Restore default preferences", message) {
						@Override
						protected void yes(DialogInterface dialog, int whichButton) {
							restoreDefaultValues(getActivity());
							getActivity().finish();
							startActivity(new Intent(getActivity(), PrefsActivity.class));
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
						sEditor.putString(BaseActivity.HISTORY_DIR_KEY, dir).commit();
					}
				}
				break;
			case BluetoothUtils.REQUEST_CONNECT_DEVICE:
				if (resultCode == RESULT_OK) {
					showToast("It's good!");
				} else if (resultCode == RESULT_CANCELED) {
					showToast("Mama mia! It's a-no good!");
				}
				break;
			}
			super.onActivityResult(requestCode, resultCode, data);
		}

		/**
		 * Restores the default app preferences to those specified by XML and {@link 
		 * PrefsActivity#DYNAMIC_PREF_DEFAULTS_MAP DYNAMIC_PREF_DEFAULTS_MAP}.
		 * @param context The {@link Context} to use.
		 */
		private static void restoreDefaultValues(Context context) {
			sEditor.clear();
			sEditor.commit();
			PreferenceManager.setDefaultValues(context, R.xml.preferences, true);
			restoreDyanmicPrefValues().commit();
		}

		/**
		 * Restores the preferences that are impossible to know before runtime to their
		 * default values.
		 * @return a {@link SharedPreferences.Editor} with the above changes. Calling
		 * {@link SharedPreferences.Editor#commit() commit()} is needed for the changes
		 * to go into effect.
		 */
		private static SharedPreferences.Editor restoreDyanmicPrefValues() {
			for (Entry<String, ? extends Object> entry : DYNAMIC_PREF_DEFAULTS_MAP.entrySet()) {
				if (entry.getValue() instanceof String) {
					sEditor.putString(entry.getKey(), (String) entry.getValue());
				} else if (entry.getValue() instanceof Boolean) {
					sEditor.putBoolean(entry.getKey(), (Boolean) entry.getValue());
				} else if (entry.getValue() instanceof Integer) {
					sEditor.putInt(entry.getKey(), (Integer) entry.getValue());
				} else if (entry.getValue() instanceof Float) {
					sEditor.putFloat(entry.getKey(), (Float) entry.getValue());
				} else if (entry.getValue() instanceof Long) {
					sEditor.putLong(entry.getKey(), (Long) entry.getValue());
				}
			}
			return sEditor;
		}

		/**
		 * Utility method for easily showing a quick message to the user.
		 * @param message The message to display.
		 */
		private void showToast(String message) {
			Toast.makeText(getActivity(), message, Toast.LENGTH_SHORT).show();
		}
	}
}