package com.kufpg.armatus;

import pl.polidea.treeview.demo.TreeListViewDemo;

import com.kufpg.armatus.console.ConsoleActivity;
import com.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import com.kufpg.armatus.util.StickyButton;

import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

public class MainActivity extends BaseActivity {
	private TextView mButtonsView;
	private StickyButton mStickyButton;
	private Button mUnstickButton, mTreeButton, mConsoleButton, mPinchZoomButton, mTerminalButton;
	private int mNumTextChanges = 0;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main_activity);

		mButtonsView = (TextView) findViewById(R.id.code_text_view);
		mStickyButton = (StickyButton) findViewById(R.id.lock_button);
		setCodeText(mNumTextChanges);
		mUnstickButton = (Button) findViewById(R.id.unlock_button);
		mTreeButton = (Button) findViewById(R.id.tree_button);
		mConsoleButton = (Button) findViewById(R.id.console_button);
		mPinchZoomButton = (Button) findViewById(R.id.pinchzoom_button);
		mTerminalButton = (Button) findViewById(R.id.terminal_activity_button);
		
		mStickyButton.setOnClickListener(new OnClickListener() {

			@Override
			public void onClick(View v) {
				mNumTextChanges++;
				setCodeText(mNumTextChanges);
			}
			
		});

		mUnstickButton.setOnClickListener(new OnClickListener() {

			@Override
			public void onClick(View v) {
				mStickyButton.unstick();
				setCodeText(mNumTextChanges);
			}	
		});

		mTreeButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				startActivity(new Intent(MainActivity.this, TreeListViewDemo.class));
			}
		});

		mConsoleButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				getEditManager().discardAllEdits();
				startActivity(new Intent(MainActivity.this, ConsoleActivity.class));
			}
		});

		mPinchZoomButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				startActivity(new Intent(MainActivity.this, TextSizePinchZoomActivity.class));
			}
		});

		mTerminalButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				String packageName = "jackpal.androidterm";
				boolean installed = appInstalledOrNot(MainActivity.this, packageName);  
				if (installed) {
					Intent i = new Intent("jackpal.androidterm.RUN_SCRIPT");
					i.addCategory(Intent.CATEGORY_DEFAULT);
					i.putExtra("jackpal.androidterm.iInitialCommand", "echo 'Hello, Armatus!'");
					startActivity(i);
				} else {
					TerminalNotInstalledDialog tnid = new TerminalNotInstalledDialog();
					tnid.show(getFragmentManager(), "tnid");
				}
			}
		});
	}
	
	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putInt("numTextChanges", mNumTextChanges);
	}
	
	@Override
	protected void onRestoreInstanceState(Bundle savedInstanceState) {
		super.onRestoreInstanceState(savedInstanceState);
		mNumTextChanges = savedInstanceState.getInt("numTextChanges");
		setCodeText(mNumTextChanges);
	}

	private void setCodeText(int numTextChanges) {
		mButtonsView.setText("Button pushed " + numTextChanges + " times. (Status: "
				+ (mStickyButton.isStuck() ? "locked" : "unlocked") + ".)");
	}

}
