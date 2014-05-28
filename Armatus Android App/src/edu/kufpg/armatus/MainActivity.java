package edu.kufpg.armatus;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.tutorial.TutorialConsoleActivity;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import edu.kufpg.armatus.treelistview.TreeListViewDemo;
import edu.kufpg.armatus.util.StickyButton;

/**
 * The {@link Activity} that is opened when the app is first started. This is merely a
 * crossroads for all of the various demos of features that may one day make it into the
 * final Armatus app.
 */
public class MainActivity extends BaseActivity {
	private TextView mButtonsView;
	private StickyButton mStickyButton;
	private Button  mConsoleButton, mTutorialButton, mUnstickButton, mTreeButton, mTerminalButton;
	private int mNumTextChanges = 0;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main_activity);

		mButtonsView = (TextView) findViewById(R.id.code_text_view);
		mConsoleButton = (Button) findViewById(R.id.console_button);
		mTutorialButton = (Button) findViewById(R.id.tutorial_button);
		mStickyButton = (StickyButton) findViewById(R.id.lock_button);
		mUnstickButton = (Button) findViewById(R.id.unlock_button);
		mTreeButton = (Button) findViewById(R.id.tree_button);
		mTerminalButton = (Button) findViewById(R.id.terminal_activity_button);

		final OnClickListener listener = new OnClickListener() {
			@Override
			public void onClick(View v) {
				switch (v.getId()) {
				case R.id.console_button:
					startActivity(new Intent(MainActivity.this, ConsoleActivity.class));
					break;
				case R.id.tutorial_button:
					startActivity(new Intent(MainActivity.this, TutorialConsoleActivity.class));
					break;
				case R.id.lock_button:
					mNumTextChanges++;
					setCodeText(mNumTextChanges);
					break;
				case R.id.unlock_button:
					mStickyButton.unstick();
					setCodeText(mNumTextChanges);
					break;
				case R.id.tree_button:
					startActivity(new Intent(MainActivity.this, TreeListViewDemo.class));
					break;
				case R.id.terminal_activity_button:
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
					break;
				}
			}
		};
		mConsoleButton.setOnClickListener(listener);
		mTutorialButton.setOnClickListener(listener);
		mStickyButton.setOnClickListener(listener);
		mUnstickButton.setOnClickListener(listener);
		mTreeButton.setOnClickListener(listener);
		mTerminalButton.setOnClickListener(listener);
		
		setCodeText(mNumTextChanges);

//		mConsoleButton.setOnClickListener(new OnClickListener() {
//			@Override
//			public void onClick(View v) {
//				startActivity(new Intent(MainActivity.this, ConsoleActivity.class));
//			}
//		});
//
//		mStickyButton.setOnClickListener(new OnClickListener() {
//			@Override
//			public void onClick(View v) {
//				mNumTextChanges++;
//				setCodeText(mNumTextChanges);
//			}
//		});
//
//		mUnstickButton.setOnClickListener(new OnClickListener() {
//			@Override
//			public void onClick(View v) {
//				mStickyButton.unstick();
//				setCodeText(mNumTextChanges);
//			}	
//		});
//
//		mTreeButton.setOnClickListener(new OnClickListener() {
//			@Override
//			public void onClick(View v) {
//				startActivity(new Intent(MainActivity.this, TreeListViewDemo.class));
//			}
//		});
//
//		mTerminalButton.setOnClickListener(new OnClickListener() {
//			@Override
//			public void onClick(View v) {
//				String packageName = "jackpal.androidterm";
//				boolean installed = appInstalledOrNot(MainActivity.this, packageName);  
//				if (installed) {
//					Intent i = new Intent("jackpal.androidterm.RUN_SCRIPT");
//					i.addCategory(Intent.CATEGORY_DEFAULT);
//					i.putExtra("jackpal.androidterm.iInitialCommand", "echo 'Hello, Armatus!'");
//					startActivity(i);
//				} else {
//					TerminalNotInstalledDialog tnid = new TerminalNotInstalledDialog();
//					tnid.show(getFragmentManager(), "tnid");
//				}
//			}
//		});
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
