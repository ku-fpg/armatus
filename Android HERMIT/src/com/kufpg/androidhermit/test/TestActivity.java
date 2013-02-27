package com.kufpg.androidhermit.test;

import java.util.ArrayList;
import java.util.Arrays;

import com.djpsoft.moreDroid.ExpandoLayout;
import com.kufpg.androidhermit.MainActivity;
import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.StandardListActivity;
import com.kufpg.androidhermit.console.CommandDispatcher;
import com.kufpg.androidhermit.drag.CommandLayout;
import com.slidingmenu.lib.SlidingMenu;

import android.app.DialogFragment;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.Intent;
import android.content.res.Configuration;
import android.graphics.Typeface;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.ContextMenu;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.View.OnKeyListener;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

/**
 * Activity that displays an interactive, feature-rich
 * (at least it will be some day) HERMIT console.
 */
public class TestActivity extends StandardListActivity {

	public static final String COMMAND_LAYOUT = "layout";
	public static final String TYPEFACE = "fonts/DroidSansMonoDotted.ttf";
	public static final String WHITESPACE = "\\s+";
	public static final int DEFAULT_FONT_SIZE = 15;
	public static final int PADDING = 5;
	public static final int ENTRY_LIMIT = 50;
	public static final int SELECT_ID = 19;
	public static final int DIALOG_ID = 20;

	private ListView mListView;
	private TestConsoleEntryAdapter mAdapter;
	private ArrayList<TestConsoleEntry> mEntries = new ArrayList<TestConsoleEntry>();
	private View mInputView, mRootView;
	private TextView mInputNum;
	private EditText mInputEditText;
	private SlidingMenu mSlidingMenu;
	private LinearLayout mExpandoLayoutGroup;
	private TestCommandDispatcher mDispatcher;
	private String mTempCommand;
	private boolean mIsSoftKeyboardVisible;
	private int mEntryCount = 0;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.test_activity);

		//Ensures soft keyboard is opened on activity start
		setSoftKeyboardVisibility(mIsSoftKeyboardVisible = true);
		mRootView = ((ViewGroup) findViewById(android.R.id.content)).getChildAt(0);
		mRootView.getViewTreeObserver().addOnGlobalLayoutListener(new OnGlobalLayoutListener() {
			@Override
			//Detects whether soft keyboard is open or closed
			public void onGlobalLayout() {
				int rootHeight = mRootView.getRootView().getHeight();
				int heightDiff = rootHeight - mRootView.getHeight();
				if (heightDiff > rootHeight/3) { //This works on Nexus 7s, at the very least
					mIsSoftKeyboardVisible = true;
				} else {
					mIsSoftKeyboardVisible = false;
				}
			}
		});

		mDispatcher = new TestCommandDispatcher(this);
		mAdapter = new TestConsoleEntryAdapter(this, mEntries);
		mListView = getListView();
		//TODO: Make mListView scroll when CommandIcons are dragged near boundaries
		registerForContextMenu(mListView);
		mInputView = getLayoutInflater().inflate(R.layout.console_input, null);
		mInputNum = (TextView) mInputView.findViewById(R.id.test_code_input_num);
		mInputNum.setText("hermit<" + mEntryCount + "> ");
		mInputEditText = (EditText) mInputView.findViewById(R.id.test_code_input_edit_text);
		mInputEditText.addTextChangedListener(new TextWatcher() {
			@Override
			public void afterTextChanged(Editable s) {}
			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {}
			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
				//Closes SlidingMenu and scroll to bottom if user begins typing
				mSlidingMenu.showContent();
				mListView.post(new Runnable() {
					public void run() {
						//DON'T use scrollToBottom(); it will cause a strange jaggedy scroll effect
						setSelection(mListView.getCount() - 1);
					}
				});
			}
		});
		//Processes user input (and runs command, if input is a command) when Enter is pressed
		mInputEditText.setOnKeyListener(new OnKeyListener() {
			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP) {
					String[] inputs = mInputEditText.getText().
							toString().trim().split(WHITESPACE);
					if (CommandDispatcher.isCommand(inputs[0])) {
						if (inputs.length == 1) {
							mDispatcher.runOnConsole(inputs[0]);
						} else {
							mDispatcher.runOnConsole(inputs[0], Arrays.copyOfRange
									(inputs, 1, inputs.length));
						}
					} else {
						addEntry(mInputEditText.getText().toString());
					}
					mInputEditText.setText(""); 
					return true;
				}
				return false;
			}
		});
		mInputEditText.requestFocus();
		mListView.addFooterView(mInputView, null, false);
		setListAdapter(mAdapter); //MUST be called after addFooterView()
		updateEntries();

		//Typeface tinkering
		Typeface typeface = Typeface.createFromAsset(getAssets(), TYPEFACE);
		mInputNum.setTypeface(typeface);
		mInputEditText.setTypeface(typeface);

		//Initialize SlidingMenu properties
		mSlidingMenu = new SlidingMenu(this);
		mSlidingMenu.setMode(SlidingMenu.LEFT);
		mSlidingMenu.setTouchModeAbove(SlidingMenu.TOUCHMODE_FULLSCREEN);
		mSlidingMenu.setFadeDegree(0.35f);
		mSlidingMenu.setShadowWidthRes(R.dimen.shadow_width);
		mSlidingMenu.attachToActivity(this, SlidingMenu.SLIDING_CONTENT);
		mSlidingMenu.setMenu(R.layout.drag_n_drop);
		refreshSlidingMenu();

		//Creates the sliding menu and iterates through the CommandLayouts
		//in drap_n_drop.xml to link them to mSlidingMenu in this activity
		int commandLayoutCount = 0;
		mExpandoLayoutGroup = (LinearLayout) findViewById(R.id.expando_root_layout);
		//Increment by two, since each CommandLayout is separated by a divider View
		for(int i = 0; i < mExpandoLayoutGroup.getChildCount(); i += 2) {
			//Retrieve child 1 from ExpandoLayout because there is an implicit child 0 that is not viewable.
			//We found this out by accident and random number changing. We recommend not changing this code.
			commandLayoutCount += ((RelativeLayout) ((ExpandoLayout) mExpandoLayoutGroup.
					getChildAt(i)).getChildAt(1)).getChildCount(); 
		}	
		for (int i = 1; i <= commandLayoutCount; i++) {
			String layoutId = COMMAND_LAYOUT + i;
			int resId = getResources().getIdentifier(layoutId, "id", "com.kufpg.androidhermit");
			((CommandLayout) mSlidingMenu.getMenu().findViewById(resId)).setSlidingMenu(mSlidingMenu);
		}
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString("Input", mInputEditText.getText().toString());
		outState.putInt("CursorPos", mInputEditText.getSelectionStart());
		outState.putInt("EntryCount", mEntryCount);
		outState.putBoolean("SoftKeyboardVisibility", mIsSoftKeyboardVisible);
		outState.putSerializable("Entries", mEntries);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onRestoreInstanceState(Bundle state) {
		super.onRestoreInstanceState(state);
		mInputEditText.setText(state.getString("Input"));
		mInputEditText.setSelection(state.getInt("CursorPos"));
		mInputEditText.requestFocus();
		mEntryCount = state.getInt("EntryCount");
		mIsSoftKeyboardVisible = state.getBoolean("SoftKeyboardVisibility");
		mEntries = (ArrayList<TestConsoleEntry>) state.getSerializable("Entries");
		mAdapter = new TestConsoleEntryAdapter(this, mEntries);
		setListAdapter(mAdapter);
		updateEntries();
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		AdapterContextMenuInfo info = (AdapterContextMenuInfo) menuInfo;
		if (info.position != mEntries.size()) { //To prevent footer from spawning a ContextMenu
			super.onCreateContextMenu(menu, v, menuInfo);
			int order = 0;
			if (mTempCommand != null) { //If user dragged CommandIcon onto entry
				menu.setHeaderTitle("Execute " + mTempCommand + " on...");
			} else { //If user long-clicked entry
				menu.setHeaderTitle(R.string.context_menu_title);
				menu.add(0, SELECT_ID, 0, "Select contents");
				order = 1;
			}
			for (String keyword : mEntries.get(info.position).getKeywords()) {
				menu.add(0, v.getId(), order, keyword);
				order++;
			}
		}
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		if (item != null) {
			if (item.getItemId() == SELECT_ID) {
				AdapterContextMenuInfo info = (AdapterContextMenuInfo) item.getMenuInfo();
				int entryNum = mEntries.get(info.position).getNum();
				String entryContents = mEntries.get(info.position).getContents();
				showEntryDialog(entryNum, entryContents);
			} else {
				String keywordNStr = item.getTitle().toString();
				if (mTempCommand != null) { //If CommandIcon command is run
					mDispatcher.runOnConsole(mTempCommand, keywordNStr);
				} else { //If keyword command is run
					mDispatcher.runKeywordCommand(keywordNStr, keywordNStr); 
				}
				mInputEditText.requestFocus(); //Prevents ListView from stealing focus
			}
		}
		mTempCommand = null;
		return super.onContextItemSelected(item);
	}

	@Override
	public void onContextMenuClosed(Menu menu) {
		//Ensures that the temp variables do not persist to next context menu opening
		mTempCommand = null;
	}

	/**
	 * Adds a new entry to the console.
	 * @param contents The message to be shown in the entry.
	 */
	public void addEntry(String contents) {
		TestConsoleEntry ce = new TestConsoleEntry(contents, mEntryCount);
		mEntries.add(ce);
		updateEntries();
		mEntryCount++;
		mInputNum.setText("hermit<" + mEntryCount + "> ");
		scrollToBottom();
	}

	/**
	 * Appends a newline and newContents to the most recent entry.
	 * @param newContents The message to be appended to the entry.
	 */
	public void appendEntry(String newContents) {
		String contents = mEntries.get(mEntries.size() - 1).getContents();
		contents += "<br />" + newContents;
		mEntries.remove(mEntries.size() - 1);
		/* Use 1 less than mEntryCount, since we are retroactively modifying an entry after
		 * addEntry() was called (which incremented mEntryCount) */
		TestConsoleEntry ce = new TestConsoleEntry(contents, mEntryCount - 1);
		mEntries.add(ce);
		updateEntries();
		scrollToBottom();
	}

	/**
	 * Removes all console entries and resets the entry count.
	 */
	public void clear() {
		mEntries.clear();
		mEntryCount = 0;
		updateEntries();
	}

	/**
	 * Exits the console activity.
	 */
	public void exit() {
		finish();
		startActivity(new Intent(this, MainActivity.class));
	}

	/**
	 * Sets the name of the Command to be run on a keyword when selected from a ContextMenu.
	 * Intended to be used in conjunction with CommandIcon.
	 * @param commandName The name of the Command that will be run (if selected).
	 */
	public void setTempCommand(String commandName) {
		mTempCommand = commandName;
	}

	/**
	 * Changes the SlidingMenu offset depending on which screen orientation is enabled.
	 * This probably works best on Nexus 7s.
	 */
	private void refreshSlidingMenu() {
		if (getResources().getConfiguration().orientation == Configuration.ORIENTATION_PORTRAIT) {
			mSlidingMenu.setBehindOffsetRes(R.dimen.slidingmenu_offset_portrait);
		} else {
			mSlidingMenu.setBehindOffsetRes(R.dimen.slidingmenu_offset_landscape);
		}
	}

	/**
	 * Show the entry at the bottom of the console ListView.
	 */
	private void scrollToBottom() {
		mListView.post(new Runnable() {
			public void run() {
				setSelection(mListView.getCount());
				mListView.smoothScrollToPosition(mListView.getCount());
			}
		});
	}

	/**
	 * Shows or hides the soft keyboard.
	 * @param visibility Set to true to show soft keyboard, false to hide.
	 */
	private void setSoftKeyboardVisibility(boolean visibility) {
		if (visibility) {
			getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
		} else {
			getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		}
	}
	
	private void showEntryDialog(int entryNum, String entryContents) {
		FragmentTransaction ft = getFragmentManager().beginTransaction();
		Fragment prev = getFragmentManager().findFragmentByTag("dialog");
		if (prev != null) {
			ft.remove(prev);
		}
		ft.addToBackStack(null);
		
		DialogFragment newFrag = TestConsoleDialog.newInstance(entryNum, entryContents);
		newFrag.show(ft, "dialog");
	}

	/**
	 * Refreshes the console entries, removing excessive entries from the top if ENTRY_LIMIT is exceeded.
	 */
	private void updateEntries() {
		if (mEntries.size() > ENTRY_LIMIT) {
			mEntries.remove(0);
		}
		mAdapter.notifyDataSetChanged();
		mInputNum.setText("hermit<" + mEntryCount + "> ");
	}

}
