package com.kufpg.androidhermit.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.djpsoft.moreDroid.ExpandoLayout;
import com.kufpg.androidhermit.MainActivity;
import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.StandardActivity;
import com.kufpg.androidhermit.console.CommandDispatcher;
import com.kufpg.androidhermit.drag.CommandLayout;
import com.slidingmenu.lib.SlidingMenu;

import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.ContextMenu;
import android.view.KeyEvent;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.View.OnKeyListener;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.widget.AdapterView;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.AdapterView.OnItemLongClickListener;

public class TestActivity extends StandardActivity {

	public static final String COMMAND_LAYOUT = "layout";
	public static final String TYPEFACE = "fonts/DroidSansMonoDotted.ttf";
	public static final String WHITESPACE = "\\s+";
	public static final int DEFAULT_FONT_SIZE = 15;
	public static final int PADDING = 5;
	public static final int LINE_LIMIT = 50;

	private ListView mListView;
	private TestConsoleEntryAdapter mAdapter;
	private ArrayList<TestConsoleEntry> mEntries = new ArrayList<TestConsoleEntry>();
	private TextView mInputNum;
	private EditText mInputEditText;
	private SlidingMenu mSlidingMenu;
	private LinearLayout mExpandoLayoutGroup;
	private TestCommandDispatcher mDispatcher;
	private String mTempCommand;
	private List<String> mTempKeywords = new ArrayList<String>();
	private View mRootView;
	private boolean mIsSoftKeyboardVisible;
	private int mEntryCount = 0;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.test_activity);

		setSoftKeyboardVisibility(mIsSoftKeyboardVisible = true);
		mRootView = ((ViewGroup) findViewById(android.R.id.content)).getChildAt(0);
		mRootView.getViewTreeObserver().addOnGlobalLayoutListener(new OnGlobalLayoutListener() {
			@Override
			//Used for detecting whether soft keyboard is open or closed
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
		mListView = (ListView) findViewById(R.id.code_list_view);
		mListView.setOnItemLongClickListener(new OnItemLongClickListener() {
			@Override
			public boolean onItemLongClick(AdapterView<?> parent,
					View view, int position, long id) {
				List<String> keywords = mAdapter.getItem(position).getKeywords();
				if (!keywords.isEmpty()) {
					setTempCommand(null);
					setTempKeywords(keywords);
					//openContextMenu(view);
					return true;
				}
				return false;
			}
		});
		mInputNum = (TextView) findViewById(R.id.test_code_input_num);
		mInputNum.setText("hermit<" + mEntryCount + "> ");
		mInputEditText = (EditText) findViewById(R.id.test_code_input_edit_text);
		mInputEditText.addTextChangedListener(new TextWatcher() {
			@Override
			public void afterTextChanged(Editable s) {}
			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {}
			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
				mSlidingMenu.showContent();
			}
		});
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
						addMessage(mInputEditText.getText().toString());
					}
					mInputEditText.setText(""); 
					return true;
				}
				return false;
			}
		});
		updateEntries();

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
		outState.putInt("EntryCount", mEntryCount);
		outState.putBoolean("SoftKeyboardVisibility", mIsSoftKeyboardVisible);
		outState.putSerializable("Entries", mEntries);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onRestoreInstanceState(Bundle savedInstanceState) {
		super.onRestoreInstanceState(savedInstanceState);
		mEntryCount = savedInstanceState.getInt("EntryCount");
		mIsSoftKeyboardVisible = savedInstanceState.getBoolean("SoftKeyboardVisibility");
		mEntries = (ArrayList<TestConsoleEntry>) savedInstanceState.getSerializable("Entries");
		updateEntries();
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		//TODO: Add Adam's clipboard stuff here later
		super.onCreateContextMenu(menu, v, menuInfo);
		if (mTempCommand != null) {
			menu.setHeaderTitle("Execute " + mTempCommand + " on...");
		} else {
			menu.setHeaderTitle(R.string.context_menu_title);
		}
		int order = 0;
		for (String keyword : mTempKeywords) {
			menu.add(0, v.getId(), order, keyword);
			order++;
		}
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		//TODO; Add Adam's clipboard stuff here later
		if (item != null) {
			String keywordNStr = item.getTitle().toString();
			if (mTempCommand != null) {
				mDispatcher.runOnConsole(mTempCommand, keywordNStr);
			} else {
				mDispatcher.runKeywordCommand(keywordNStr, keywordNStr); 
			}
		}
		return super.onContextItemSelected(item);
	}

	/**
	 * Adds a new "line" to the console with msg as its contents.
	 * @param msg
	 */
	public void addMessage(String msg) {
		TestConsoleEntry ce = new TestConsoleEntry(msg, mEntryCount);
		mEntries.add(ce);
		updateEntries();
		mEntryCount++;
		mListView.post(new Runnable() {
			public void run() {
				mListView.setSelection(mListView.getCount() - 1);
			}
		});
		mInputNum.setText("hermit<" + mEntryCount + "> ");
	}

	public void clear() {
		mEntries.clear();
		mAdapter.notifyDataSetChanged();
		mEntryCount = 0;
		mInputNum.setText("hermit<" + mEntryCount + "> ");
	}

	public void exit() {
		finish();
		startActivity(new Intent(this, MainActivity.class));
	}

	public void setTempCommand(String commandName) {
		mTempCommand = commandName;
	}

	public void setTempKeywords(List<String> keywords) {
		mTempKeywords = keywords;
	}

	private void refreshSlidingMenu() {
		if (getResources().getConfiguration().orientation == Configuration.ORIENTATION_PORTRAIT) {
			mSlidingMenu.setBehindOffsetRes(R.dimen.slidingmenu_offset_portrait);
		} else {
			mSlidingMenu.setBehindOffsetRes(R.dimen.slidingmenu_offset_landscape);
		}
	}

	private void setSoftKeyboardVisibility(boolean visibility) {
		if (visibility) {
			getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_VISIBLE);
		} else {
			getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		}
	}

	private void updateEntries() {
		if (mEntries.size() > LINE_LIMIT) {
			mEntries.remove(0);
		}
		mAdapter = new TestConsoleEntryAdapter(this, mEntries);
		mListView.setAdapter(mAdapter);
		mInputNum.setText("hermit<" + mEntryCount + "> ");
	}

}
