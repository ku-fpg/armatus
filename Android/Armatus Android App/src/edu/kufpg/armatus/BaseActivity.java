package edu.kufpg.armatus;

import edu.kufpg.armatus.Prefs.Theme;
import android.app.ActionBar;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.os.Bundle;
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
	
	/** Tracks the ID of the current application theme. */
	private static Theme sThemeId;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		Constants.initConstants(this);
		Prefs.initPrefs(this);
		sThemeId = Prefs.getTheme(this);
		Prefs.setTheme(this, sThemeId);
		
		super.onCreate(savedInstanceState);
		ActionBar actionBar = getActionBar();
		actionBar.show();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.console_list_view_menu, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.undo:
			return true;
		case R.id.redo:
			return true;
		case R.id.menu_settings:
			Intent settingsActivity = new Intent(this, PrefsActivity.class);
			startActivity(settingsActivity);
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	protected void onResume() {
		//If the theme has changed while navigating the back stack
		if (!sThemeId.equals(Prefs.getTheme(this))) {
			recreate();
		}

		super.onResume();
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
		if (message != null) {
			showToast(message.toString());
		} else {
			showToast("null");
		}
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

}
