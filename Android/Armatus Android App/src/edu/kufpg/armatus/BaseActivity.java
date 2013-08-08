package edu.kufpg.armatus;

import java.util.Map;
import java.util.Map.Entry;

import com.google.common.collect.ImmutableMap;
import edu.kufpg.armatus.EditManager.OnEditListener;

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

/**
 * The {@link Activity} which all other Activities should extend. BaseActivity has utilities such as
 * preference keys, a central {@link EditManager}, {@link SharedPreferences}, and the ability to
 * detect a theme change (and restart).
 */
public class BaseActivity extends Activity {

	/** The directory where any persistent data should be saved. */
	public static final String CACHE_DIR = Environment.getExternalStorageDirectory().getPath() + "/data/armatus";

	/** The package name as specified in the Android Manifest file. */
	public static String PACKAGE_NAME;

	/**
	 * {@link android.preference.CheckBoxPreference CheckBoxPreference} key mapping to whether or
	 * not {@link #CACHE_DIR} should be used to save persistent data. If not, the String to which
	 * {@link #HISTORY_DIR_KEY} maps is used instead.
	 */
	public static String HISTORY_USE_CACHE_KEY;

	/**
	 * {@link android.preference.Preference Preference} key mapping to the String representation
	 * of a directory where persistent data can be stored. The directory is only used if the value
	 * to which {@link #HISTORY_USE_CACHE_KEY} maps is true.
	 */
	public static String HISTORY_DIR_KEY;

	/**
	 * {@link android.preference.ListPreference ListPreference} key mapping to one of three String
	 * values: "0" (for {@link BaseActivity.EditMode#READ READ} mode), "1" (for {@link BaseActivity.
	 * EditMode#WRITE WRITE} mode), or "2" (for {@link BaseActivity.EditMode#ARITHMETIC ARITHMETIC}
	 * mode). The mapped String represent which {@link BaseActivity.EditMode EditMode} is currently
	 * being used.
	 */
	public static String EDIT_MODE_KEY;

	/**
	 * {@link android.preference.ListPreference ListPreference} key mapping to either {@link
	 * #APP_THEME_DARK} or {@link #APP_THEME_LIGHT}, depending on which theme is currently
	 * being used.
	 */
	public static String APP_THEME_KEY;
	
	/**
	 * One of the possible values that the {@link android.preference.Preference Preference} to
	 * which {@link #APP_THEME_KEY} maps can be (the other being {@link #APP_THEME_LIGHT}).
	 */
	public static String APP_THEME_DARK;

	/**
	 * One of the possible values that the {@link android.preference.Preference Preference} to
	 * which {@link #APP_THEME_KEY} maps can be (the other being {@link #APP_THEME_DARK}).
	 */
	public static String APP_THEME_LIGHT;
	
	/**
	 * {@link android.preference.ListPreference ListPreference} key mapping to either {@link
	 * #NETWORK_SOURCE_WEB_SERVER} or {@link #NETWORK_SOURCE_BLUETOOTH_SERVER}, depending on
	 * which network source is currently being used.
	 */
	public static String NETWORK_SOURCE_KEY;
	
	/**
	 * One of the possible values that the {@link android.preference.Preference Preference} to
	 * which {@link #NETWORK_SOURCE_KEY} maps can be (the other being {@link
	 * #NETWORK_SOURCE_BLUETOOTH_SERVER}).
	 */
	public static String NETWORK_SOURCE_WEB_SERVER;
	
	/**
	 * One of the possible values that the {@link android.preference.Preference Preference} to
	 * which {@link #NETWORK_SOURCE_KEY} maps can be (the other being {@link
	 * #NETWORK_SOURCE_WEB_SERVER}).
	 */
	public static String NETWORK_SOURCE_BLUETOOTH_SERVER;

	/**
	 * Maps special {@link android.preference.Preference Preference} keys to their default values
	 * when the default values are impossible to know before runtime (e.g., the external cache
	 * directory, which {@link #HISTORY_USE_CACHE_KEY} maps to by default).
	 */
	private static Map<String, ? extends Object> DYNAMIC_PREF_DEFAULTS_MAP;

	/** Used to access persistent user preferences. Editing them requires {@link #sEditor}. */
	private static SharedPreferences sPrefs;

	/** Used to edit persistent user preferences stored in {@link #sPrefs}. */
	private static SharedPreferences.Editor sEditor;

	/** An application-wide edit manager. */
	private static EditManager sEditManager = new EditManager();

	/** MenuItem which undoes edits stored in {@link #sEditManager}. */
	private static MenuItem sUndoIcon;

	/** MenuItem which redoes edits stored in {@link #sEditManager}. */
	private static MenuItem sRedoIcon;

	/** Tracks the ID of the current application theme. */
	private static int sThemeId;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		PACKAGE_NAME = getApplicationContext().getPackageName();
		sPrefs = PreferenceManager.getDefaultSharedPreferences(this);
		sEditor = sPrefs.edit();
		HISTORY_USE_CACHE_KEY = getResources().getString(R.string.pref_history_use_cache);
		HISTORY_DIR_KEY = getResources().getString(R.string.pref_history_dir);
		EDIT_MODE_KEY = getResources().getString(R.string.pref_edit_mode);
		APP_THEME_KEY = getResources().getString(R.string.pref_app_theme);
		APP_THEME_DARK = getResources().getString(R.string.pref_app_theme_dark);
		APP_THEME_LIGHT = getResources().getString(R.string.pref_app_theme_light);
		NETWORK_SOURCE_KEY = getResources().getString(R.string.pref_network_source);
		NETWORK_SOURCE_WEB_SERVER = getResources().getString(R.string.pref_network_source_web);
		NETWORK_SOURCE_BLUETOOTH_SERVER = getResources().getString(R.string.pref_network_source_bluetooth);
		DYNAMIC_PREF_DEFAULTS_MAP = mapDynamicPrefDefaults();
		PreferenceManager.setDefaultValues(this, R.xml.preferences, true);

		//Set dynamic preferences' default values
		for (Entry<String, ? extends Object> entry : DYNAMIC_PREF_DEFAULTS_MAP.entrySet()) {
			if (entry.getValue() instanceof String) {
				if (sPrefs.getString(entry.getKey(), null) == null) {
					sEditor.putString(entry.getKey(), (String) entry.getValue());
				}
			}
		}
		sEditor.commit();

		sEditManager.setOnEditListener(new OnEditListener() {
			@Override
			public void onEditAction() {
				updateIconTitles();
			}
		});

		sThemeId = getThemePrefId();
		setTheme(sThemeId);

		super.onCreate(savedInstanceState);
		ActionBar actionBar = getActionBar();
		actionBar.show();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.console_list_view_menu, menu);
		sUndoIcon = menu.findItem(R.id.undo);
		sRedoIcon = menu.findItem(R.id.redo);
		updateIconTitles();
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.undo:
			if (canUndo()) {
				sEditManager.undo();
				updateIconTitles();
			}
			return true;
		case R.id.redo:
			if (canRedo()) {
				sEditManager.redo();
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
		//If the theme has changed while navigating the back stack
		if (sThemeId != getThemePrefId()) {
			recreate();
		}
		//
		((BaseApplication<BaseActivity>) getApplication()).attachActivity(this);
		super.onResume();
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		((BaseApplication<BaseActivity>) getApplication()).detachActivity(this);
	}

	/**
	 * Returns whether the application's {@link EditManager} is capable of redoing
	 * an action. Subclasses of {@link BaseActivity} can impose additional restrictions.
	 * @return {@code true} if the application-level {@code EditManager} is
	 * allowed to redo an action.
	 */
	public boolean canRedo() {
		return sEditManager.canRedo();
	}

	/**
	 * Returns whether the application's {@link EditManager} is capable of undoing
	 * an action. Subclasses of {@link BaseActivity} can impose additional restrictions.
	 * @return {@code true} if the application-level {@code EditManager} is
	 * allowed to undo an action.
	 */
	public boolean canUndo() {
		return sEditManager.canUndo();
	}

	/**
	 * Utility method for easily showing a quick message to the user.
	 * @param message The message to display.
	 */
	public void showToast(String message) {
		Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
	}

	/**
	 * Utility method for easily showing a quick message to the user.
	 * @param message The object that will be converted to a message by calling its
	 * {@link Object#toString() toString()} method.
	 */
	public void showToast(Object message) {
		showToast(message.toString());
	}

	/**
	 * Determines if an app is installed on the current device by looking up its package
	 * name.
	 * @param context The {@link Context} to use.
	 * @param packageName The app's package name. Make sure to pass in the full package name, or
	 * this method will not return the correct result.
	 * @return {@code true} if the app is currently installed.
	 */
	public static boolean appInstalledOrNot(Context context, String packageName) {
		PackageManager pm = context.getPackageManager();
		boolean appInstalled = false;
		try {
			pm.getPackageInfo(packageName, PackageManager.GET_ACTIVITIES);
			appInstalled = true;
		} catch (PackageManager.NameNotFoundException e){
			appInstalled = false;
		}
		return appInstalled;
	}

	/**
	 * Returns the application-level {@link EditManager}.
	 * @return the application-level {@link EditManager}.
	 */
	protected EditManager getEditManager() {
		return sEditManager;
	}

	/**
	 * Returns an instance of this app's {@link SharedPreferences}, which allow you to look
	 * up persistent preference data using their keys.
	 * @return the app's {@code SharedPreferences}.
	 */
	protected static SharedPreferences getPrefs() {
		return sPrefs;
	}

	/**
	 * Returns a {@link SharedPreferences.Editor}, which allows you to modify the app's
	 * persistent preference data. Make sure to call {@link SharedPreferences.Editor#commit()
	 * commit()} after making any changes.
	 * @return the app's {@code SharedPreferences} editor.
	 */
	protected static SharedPreferences.Editor getPrefsEditor() {
		return sEditor;
	}

	/**
	 * Returns the resource ID of the current app theme (either {@code ThemeLight} or
	 * {@code ThemeDark}.
	 * @return the current app theme's resource ID.
	 */
	protected static int getThemePrefId() {
		String theme = sPrefs.getString(APP_THEME_KEY, null);
		if (theme.equals(APP_THEME_LIGHT)) {
			return R.style.ThemeLight;
		} else if (theme.equals(APP_THEME_DARK)) {
			return R.style.ThemeDark;
		} else {
			return -1;
		}
	}

	/**
	 * Updates the titles of the undo and redo icons in the options menu.
	 */
	protected static void updateIconTitles() {
		sUndoIcon.setTitleCondensed(String.valueOf(sEditManager.getRemainingUndosCount()));
		sRedoIcon.setTitleCondensed(String.valueOf(sEditManager.getRemainingRedosCount()));
	}

	/** @return {@link #DYNAMIC_PREF_DEFAULTS_MAP}. */
	static Map<String, ? extends Object> getDyanmicPrefDefaults() {
		return DYNAMIC_PREF_DEFAULTS_MAP;
	}

	/**
	 * Initializes {@link #DYNAMIC_PREF_DEFAULTS_MAP} by mapping {@link
	 * android.preference.Preference Preference} keys to their default values when the default
	 * values are impossible to know before runtime.
	 * @return a map of {@code Preference} keys to their dynamic default values.
	 */
	private static Map<String, ? extends Object> mapDynamicPrefDefaults() {
		return ImmutableMap.of(HISTORY_DIR_KEY, CACHE_DIR);
	}

	public static enum EditMode {
		READ,
		WRITE,
		ARITHMETIC
	}

}
