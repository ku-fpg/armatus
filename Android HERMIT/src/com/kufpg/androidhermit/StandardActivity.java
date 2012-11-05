package com.kufpg.androidhermit;

import java.io.FileNotFoundException;
import java.util.ArrayList;

import com.kufpg.androidhermit.util.FileIOManager;

import android.app.ActionBar;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.Uri;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.widget.Toast;

public class StandardActivity extends Activity {

	public final static int FILE_FROM_DISK = 1;
	protected static Context mContext;
	protected static String mSaveDir;
	protected static String mDefaultSaveDir;
	protected static String mEditModeValue;
	protected static SharedPreferences prefs;
	protected static SharedPreferences.Editor prefsEditor;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		ActionBar actionBar = getActionBar();
		actionBar.show();

		mContext = getApplicationContext();
		mSaveDir = getCacheDir().toString();
		mDefaultSaveDir = mSaveDir;
		mEditModeValue = "0";
		prefs = PreferenceManager.getDefaultSharedPreferences(mContext);
		prefsEditor = prefs.edit();
		if(getSaveDir() == null) {
			loadPrefs();
		}
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.actionbar, menu);

		MenuItem item = menu.findItem(R.id.save_file);
		item.setVisible(false);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item){
		switch(item.getItemId()) {
		case R.id.open_file:
			Intent filesIntent = new Intent();
			filesIntent.setType("text/plain");
			filesIntent.setAction(Intent.ACTION_GET_CONTENT);								
			startActivityForResult(Intent.createChooser(filesIntent,
					"Select app"), FILE_FROM_DISK);
			return true;
		case R.id.menu_settings:
			Intent settingsActivity = new Intent(getBaseContext(),
					Preferences.class);
			startActivity(settingsActivity);
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	public void makeToast(String message) {
		Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
	}

	public void openCode(String code) {
		//Empty stub; override where necessary
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent intent) { 
		super.onActivityResult(requestCode, resultCode, intent);

		switch(requestCode) {
		case FILE_FROM_DISK:
			if(resultCode == RESULT_OK) {
				Uri diskTextFile = intent.getData();
				ArrayList<String> code = null;
				try {
					code = FileIOManager.getTextArray(getContentResolver().openInputStream(diskTextFile));
				} catch (FileNotFoundException e) {
					e.printStackTrace();
				}
				Intent codeIntent = new Intent(this, WarpDSLV.class);
				codeIntent.putExtra("CODE_ARRAY", code);
				String[] uriBits = intent.getDataString().replace("file:///", "").split("/"); //The replace method is due to the Nexus 7's filesystem, so this may need to be improved later
				String codePath = "";
				for(int i = 0; i < uriBits.length - 1; i++)
					codePath += "/" + uriBits[i];
				codeIntent.putExtra("CODE_PATH", codePath);
				codeIntent.putExtra("CODE_FILENAME", uriBits[uriBits.length-1]);
				startActivity(codeIntent);
			}
		}
	}

	public static String getSaveDir() {
		return mSaveDir;
	}

	public static void setSaveDir(String saveDir) {
		mSaveDir = saveDir;
	}

	public static String getDefaultSaveDir() {
		return mDefaultSaveDir;
	}

	public static String getEditModeValue() {
		return mEditModeValue;
	}

	public static void setEditModeValue(String editModeValue) {
		mEditModeValue = editModeValue;
	}

	public static void setDefaultPrefs() {
		prefsEditor.clear();
		PreferenceManager.setDefaultValues(mContext, R.xml.preferences, true);
		prefsEditor.commit();
	}

	public static void loadPrefs() {
		prefsEditor.putString("savedir_pref", mSaveDir);
		prefsEditor.putString("editmode_pref", mEditModeValue);
		prefsEditor.commit();
	}

}
