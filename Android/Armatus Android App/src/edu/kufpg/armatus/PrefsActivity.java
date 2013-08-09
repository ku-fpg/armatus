package edu.kufpg.armatus;

import java.io.File;

import com.ipaulpro.afilechooser.FileChooserActivity;
import com.ipaulpro.afilechooser.utils.FileUtils;

import edu.kufpg.armatus.dialog.YesOrNoDialog;
import edu.kufpg.armatus.server.BluetoothDeviceListActivity;
import edu.kufpg.armatus.server.BluetoothUtils;

import android.app.Activity;
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

	/**
	 * {@link Preference} key used to choose the Bluetooth device if Bluetooth communications
	 * are enabled.
	 */
	private static String CHOOSE_BLUETOOTH_DEVICE_KEY;

	/**
	 * {@link Preference} key used for resetting preferences back to their default values.
	 */
	private static String RESTORE_DEFAULTS_KEY;

	/** Used to access persistent user preferences. Editing them requires {@link #sPrefsEditor}. */
	private static SharedPreferences sPrefs;

	/** Used to edit persistent user preferences stored in {@link #sPrefs}. */
	private static SharedPreferences.Editor sPrefsEditor;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		CHOOSE_BLUETOOTH_DEVICE_KEY = getResources().getString(R.string.pref_choose_bluetooth_device);
		RESTORE_DEFAULTS_KEY = getResources().getString(R.string.pref_restore_defaults);
		sPrefs = BaseActivity.getPrefs();
		sPrefsEditor = BaseActivity.getPrefsEditor();

		setTheme(BaseActivity.getThemePrefId());
		getFragmentManager().beginTransaction().replace(android.R.id.content, new PrefsFragment()).commit();
		super.onCreate(savedInstanceState);
	}

	/**
	 * Where the user can change the app preferences.
	 */
	public static class PrefsFragment extends PreferenceFragment {
		/** Request code used for selecting a directory with aFileChooser. */
		private static final int DIR_CHANGE_CODE = 4242;

		/**
		 * Preference that adjusts whether or not {@link BaseActivity#CACHE_DIR CACHE_DIR}
		 * should be used to save persistent data (the value mapped to by {@link
		 * BaseActivity#IS_HISTORY_DIR_CUSTOM_KEY IS_HISTORY_DIR_CUSTOM_KEY}).
		 */
		private CheckBoxPreference mIsHistoryDirCustomPref;

		/**
		 * Preference that can change the String representation of a directory where persistent
		 * data can be stored (the value mapped to by {@link BaseActivity#HISTORY_DIR_KEY
		 * HISTORY_DIR_KEY}).
		 */
		private Preference mHistoryDirPref;

		/**
		 * Preference that can change the string representation of what the current {@link
		 * BaseActivity.EditMode EditMode} is (the value mapped to by {@link BaseActivity#EDIT_MODE_KEY
		 * EDIT_MODE_KEY}).
		 */
		private ListPreference mEditModePref;

		/**
		 * Preference that can change the current app theme (the value mapped to by {@link
		 * BaseActivity#APP_THEME_KEY APP_THEME_KEY}).
		 */
		private ListPreference mAppThemePref;

		/**
		 * Preference that can change the current network source (the value mapped to by
		 * {@link BaseActivity#NETWORK_SOURCE_KEY NETWORK_SOURCE_KEY}).
		 */
		private ListPreference mNetworkSourcePref;

		/**
		 * Preference that can change the Bluetooth device used if Bluetooth communications
		 * are enabled (the value mapped to by {@link BaseActivity#CHOOSE_BLUETOOTH_DEVICE_KEY
		 * CHOOSE_BLUETOOTH_DEVICE_KEY}).
		 */
		private Preference mChooseBluetoothDevicePref;

		/**
		 * Preference that can restore the default app preferences (mapped to by {@link
		 * BaseActivity#RESTORE_DEFAULTS_KEY RESTORE_DEFAULTS_KEY}).
		 */
		private Preference mRestoreDefaultsPref;

		@Override
		public void onCreate(Bundle savedInstanceState) {
			super.onCreate(savedInstanceState);
			addPreferencesFromResource(R.xml.preferences);

			//Preference initialization, summary setting, and disabling (if necessary)
			mIsHistoryDirCustomPref = (CheckBoxPreference) findPreference(BaseActivity.IS_HISTORY_DIR_CUSTOM_KEY);
			mHistoryDirPref = findPreference(BaseActivity.HISTORY_DIR_KEY);
			setHistoryDirPrefSummary(sPrefs.getBoolean(BaseActivity.IS_HISTORY_DIR_CUSTOM_KEY, false));
			mEditModePref = (ListPreference) findPreference(BaseActivity.EDIT_MODE_KEY);
			mAppThemePref = (ListPreference) findPreference(BaseActivity.APP_THEME_KEY);
			mNetworkSourcePref = (ListPreference) findPreference(BaseActivity.NETWORK_SOURCE_KEY);
			mChooseBluetoothDevicePref = (Preference) findPreference(CHOOSE_BLUETOOTH_DEVICE_KEY);
			setChooseBluetoothDevicePrefSummary(sPrefs.getString(BaseActivity.BLUETOOTH_DEVICE_NAME_KEY, null),
					sPrefs.getString(BaseActivity.BLUETOOTH_DEVICE_ADDRESS_KEY, null));
			if (sPrefs.getString(BaseActivity.NETWORK_SOURCE_KEY, null)
					.equals(BaseActivity.NETWORK_SOURCE_WEB_SERVER)) {
				mChooseBluetoothDevicePref.setEnabled(false);
			}
			mRestoreDefaultsPref = findPreference(RESTORE_DEFAULTS_KEY);

			mIsHistoryDirCustomPref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					setHistoryDirPrefSummary((Boolean) newValue);
					return true;
				}
			});

			mHistoryDirPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					Intent intent = new Intent(getActivity(), FileChooserActivity.class);
					intent.setType(FileUtils.MIME_TYPE_TEXT);
					intent.addCategory(Intent.CATEGORY_OPENABLE);
					startActivityForResult(intent, DIR_CHANGE_CODE);
					return true;
				}
			});

			mEditModePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					//TODO: Change edit mode
					return true;
				}
			});

			mAppThemePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					getActivity().recreate();
					return true;
				}
			});

			mNetworkSourcePref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
				@Override
				public boolean onPreferenceChange(Preference preference, Object newValue) {
					//TODO: Change network source
					String source = (String) newValue;
					if (source.equals(BaseActivity.NETWORK_SOURCE_WEB_SERVER)) {
						mChooseBluetoothDevicePref.setEnabled(false);
					} else if (source.equals(BaseActivity.NETWORK_SOURCE_BLUETOOTH_SERVER)) {
						mChooseBluetoothDevicePref.setEnabled(true);
					}
					return true;
				}
			});

			mChooseBluetoothDevicePref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
				@Override
				public boolean onPreferenceClick(Preference preference) {
					if (BluetoothUtils.isBluetoothEnabled(getActivity())) {
						BluetoothUtils.findDeviceName(PrefsFragment.this);
					} else {
						BluetoothUtils.enableBluetooth(PrefsFragment.this);
					}
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
							sPrefsEditor.clear();
							sPrefsEditor.commit();
							PreferenceManager.setDefaultValues(getActivity(), R.xml.preferences, true);
							BaseActivity.restoreDyanmicPrefDefaultValues().commit();

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
						sPrefsEditor.putString(BaseActivity.HISTORY_DIR_KEY, dir).commit();
					}
				}
				break;
			case BluetoothUtils.REQUEST_ENABLE_BLUETOOTH:
				if (resultCode == RESULT_OK) {
					BluetoothUtils.findDeviceName(this);
				}
				break;
			case BluetoothUtils.REQUEST_FIND_BLUETOOTH_DEVICE:
				if (resultCode == RESULT_OK) {
					String name = data.getStringExtra(BluetoothDeviceListActivity.EXTRA_DEVICE_NAME);
					String address = data.getStringExtra(BluetoothDeviceListActivity.EXTRA_DEVICE_ADDRESS);
					BluetoothUtils.setBluetoothDevice(getActivity(), name, address);
					setChooseBluetoothDevicePrefSummary(name, address);
				}
				break;
			}
			super.onActivityResult(requestCode, resultCode, data);
		}

		/**
		 * Customizes the Bluetooth device {@link Preference} caption.
		 * @param name The friendly name of the current Bluetooth device (if any).
		 * @param address The MAC address of the current Bluetooth device (if any).
		 */
		private void setChooseBluetoothDevicePrefSummary(String name, String address) {
			if (name != null && address != null) {
				mChooseBluetoothDevicePref.setSummary(name + " (" + address + ")");
			} else {
				mChooseBluetoothDevicePref.setSummary(null);
			}
		}

		/**
		 * Customizes the {@link Preference} caption for the console session history directory.
		 * @param isCustom If the a custom directory should be used.
		 */
		private void setHistoryDirPrefSummary(boolean isCustom) {
			if (isCustom) {
				mHistoryDirPref.setSummary(sPrefs.getString(BaseActivity.HISTORY_DIR_KEY, null));
			} else {
				mHistoryDirPref.setSummary(null);
			}
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