package edu.kufpg.armatus;

import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import edu.kufpg.armatus.radialmenu.RadialMenuActivity;
import edu.kufpg.armatus.treelistview.TreeListViewDemo;
import edu.kufpg.armatus.util.StickyButton;

import android.content.Intent;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.ImageSpan;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

/**
 * The {@link Activity} that is opened when the app is first started. This is merely a
 * crossroads for all of the various demos of features that may one day make it into the
 * final Armatus app.
 */
public class MainActivity extends BaseActivity {
	private TextView mButtonsView, mGlyphSpanTextView;
	private StickyButton mStickyButton;
	private Button mUnstickButton, mTreeButton, mConsoleButton,
	mPinchZoomButton, mTerminalButton, mGlyphSpanButton;
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
		mPinchZoomButton = (Button) findViewById(R.id.radialmenu_button);
		mTerminalButton = (Button) findViewById(R.id.terminal_activity_button);
		mGlyphSpanButton = (Button) findViewById(R.id.glyph_span_button);
		mGlyphSpanTextView = (TextView) findViewById(R.id.glyph_span_text_view);
		
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
				startActivity(new Intent(MainActivity.this, ConsoleActivity.class));
			}
		});

		mPinchZoomButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				startActivity(new Intent(MainActivity.this, RadialMenuActivity.class));
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
		
		mGlyphSpanButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				int selectionStart = mGlyphSpanTextView.getSelectionStart();
				int selectionEnd = mGlyphSpanTextView.getSelectionEnd();
				
				if (selectionEnd - selectionStart != 0) {
					String toastMe = "";
					for (int i = selectionStart; i < selectionEnd; i++) {
						toastMe += i;
					}
					showToast(toastMe);
				} else {
					showToast("No selection!");
				}
			}
		});
		
		int length = 5;
		for (int i = 0; i < length; i++) {
			mGlyphSpanTextView.append(" ");
		}
		Spannable textSpans = new SpannableString(mGlyphSpanTextView.getText());
		for (int i = 0; i < length; i++) {
			Drawable d = getResources().getDrawable(R.drawable.ic_launcher);
			d.setBounds(0, 0, d.getIntrinsicWidth(), d.getIntrinsicHeight());
			ImageSpan span = new ImageSpan(d, ImageSpan.ALIGN_BASELINE);
			textSpans.setSpan(span, i, i + 1, Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
		}
		mGlyphSpanTextView.setText(textSpans);
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
