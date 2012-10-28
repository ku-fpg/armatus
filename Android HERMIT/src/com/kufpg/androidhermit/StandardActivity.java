package com.kufpg.androidhermit;

import java.io.FileNotFoundException;
import java.util.ArrayList;

import com.kufpg.androidhermit.util.FileIOManager;

import android.app.ActionBar;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.widget.Toast;

public class StandardActivity extends Activity {

	public final static int FILE_FROM_DISK = 1;
	protected Context mContext = this;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		ActionBar actionBar = getActionBar();
		actionBar.show();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.actionbar, menu);
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
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				Intent codeIntent = new Intent(this, WarpDSLV.class);
				codeIntent.putExtra("CODE_ARRAY", code);
				startActivity(codeIntent);
			}
		}
	}

}
