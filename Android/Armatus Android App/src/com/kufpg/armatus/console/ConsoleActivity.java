package com.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;

import com.kufpg.armatus.R;
import com.kufpg.armatus.StandardListActivity;
import com.kufpg.armatus.console.CommandDispatcher;
import com.kufpg.armatus.dialog.ConsoleEntryRearrangeDialog;
import com.kufpg.armatus.dialog.ConsoleEntrySelectionDialog;
import com.kufpg.armatus.dialog.ConsoleExitDialog;
import com.kufpg.armatus.dialog.KeywordSwapDialog;
import com.kufpg.armatus.dialog.WordCompletionDialog;
import com.kufpg.armatus.drag.DragIcon;
import com.kufpg.armatus.drag.DragSinkListener;
import com.kufpg.armatus.server.HermitServer;
import com.slidingmenu.lib.SlidingMenu;

import android.app.DialogFragment;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.res.Configuration;
import android.content.res.TypedArray;
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
import android.widget.ExpandableListView;
import android.widget.ListView;
import android.widget.TextView;

/**
 * Activity that displays an interactive, feature-rich
 * (at least it will be some day) HERMIT console.
 */
public class ConsoleActivity extends StandardListActivity {

	public static final String DRAG_LAYOUT = "drag_layout";
	public static final String TYPEFACE = "fonts/DroidSansMonoDotted.ttf";
	public static final String WHITESPACE = "\\s+";
	public static final int DEFAULT_FONT_SIZE = 15;
	public static final int PADDING = 5;
	public static final int ENTRY_CONSOLE_LIMIT = 100;
	public static final int ENTRY_COMMAND_HISTORY_LIMIT = 200;
	public static final String SELECTION_TAG = "selection";
	public static final String REARRANGE_TAG = "rearrange";
	public static final String KEYWORD_SWAP_TAG = "keywordswap";
	public static final String WORD_COMPLETION_TAG = "wordcomplete";

	private static final int REARRANGE_ID = 19;

	private ListView mConsoleListView, mCommandHistoryListView;
	private ExpandableListView mCommandExpandableMenuView;
	private ConsoleEntryAdapter mConsoleAdapter;
	private CommandHistoryAdapter mCommandHistoryAdapter;
	private CommandExpandableMenuAdapter mCommandExpandableMenuAdapter;
	private ArrayList<ConsoleEntry> mConsoleEntries = new ArrayList<ConsoleEntry>(); 
	private ArrayList<String> mCommandHistoryEntries = new ArrayList<String>();
	private ArrayList<String> mCommandExpandableGroups = new ArrayList<String>();
	private LinkedHashMap<String, ArrayList<String>> mCommandExpandableGroupMap = new LinkedHashMap<String, ArrayList<String>>();
	private View mInputView, mRootView;
	private TextView mInputNum;
	private EditText mInputEditText;
	private SlidingMenu mSlidingMenu;
	private CommandDispatcher mDispatcher;
	private WordCompleter mCompleter;
	private HermitServer mServer;
	private String mTempCommand;
	private boolean mInputEnabled = true;
	private boolean mSoftKeyboardVisible;
	private int mEntryCount = 0;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console_activity);

		//Ensures soft keyboard is opened on activity start
		setSoftKeyboardVisibility(mSoftKeyboardVisible = true);
		mRootView = ((ViewGroup) findViewById(android.R.id.content)).getChildAt(0);
		mRootView.getViewTreeObserver().addOnGlobalLayoutListener(new OnGlobalLayoutListener() {
			@Override
			//Detects whether soft keyboard is open or closed
			public void onGlobalLayout() {
				int rootHeight = mRootView.getRootView().getHeight();
				int heightDiff = rootHeight - mRootView.getHeight();
				if (heightDiff > rootHeight/3) { //This works on Nexus 7s, at the very least
					mSoftKeyboardVisible = true;
				} else {
					mSoftKeyboardVisible = false;
				}
			}
		});

		mDispatcher = new CommandDispatcher(this);
		mConsoleAdapter = new ConsoleEntryAdapter(this, mConsoleEntries);
		mConsoleListView = getListView();
		//TODO: Make mListView scroll when CommandIcons are dragged near boundaries
		registerForContextMenu(mConsoleListView);
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
				String input = mInputEditText.getText().toString().trim();
				if (input.split(WHITESPACE).length <= 1) {
					mCompleter.filterDictionary(input);
				}
				mConsoleListView.post(new Runnable() {
					public void run() {
						//DON'T use scrollToBottom(); it will cause a strange jaggedy scroll effect
						setSelection(mConsoleListView.getCount() - 1);
					}
				});
			}
		});
		//Processes user input (and runs command, if input is a command) when Enter is pressed
		mInputEditText.setOnKeyListener(new OnKeyListener() {
			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP
						&& mInputEnabled) {
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
						addConsoleEntry(mInputEditText.getText().toString());
					}
					mInputEditText.setText("");
					return true;
				} else if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP
						&& !mInputEnabled) {
					mInputEditText.setText("");
				}
				return false;
			}
		});
		mInputEditText.setOnDragListener(new DragSinkListener() {
			@Override
			public void onDragStarted(View dragView, View dragSink) {
				getSlidingMenu().showContent();
			}
		});
		mInputEditText.requestFocus();
		mConsoleListView.addFooterView(mInputView, null, false);
		setListAdapter(mConsoleAdapter); //MUST be called after addFooterView()
		updateConsoleEntries();

		//Typeface tinkering
		Typeface typeface = Typeface.createFromAsset(getAssets(), TYPEFACE);
		mInputNum.setTypeface(typeface);
		mInputEditText.setTypeface(typeface);

		mSlidingMenu = (SlidingMenu) findViewById(R.id.console_sliding_menu);
		refreshSlidingMenu();

		mCommandHistoryListView = (ListView)findViewById(R.id.History);
		mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistoryEntries);
		mCommandHistoryListView.setAdapter(mCommandHistoryAdapter);
		updateCommandHistoryEntries();

		loadExpandableMenuData();
		mCompleter = new WordCompleter(this, mCommandExpandableGroupMap.values());

		mCommandExpandableMenuView = (ExpandableListView) findViewById(R.id.command_expandable_menu);
		mCommandExpandableMenuAdapter = new CommandExpandableMenuAdapter
				(this, mCommandExpandableGroups, mCommandExpandableGroupMap);
		mCommandExpandableMenuView.setAdapter(mCommandExpandableMenuAdapter);
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString("input", mInputEditText.getText().toString());
		outState.putInt("cursorPos", mInputEditText.getSelectionStart());
		outState.putInt("entryCount", mEntryCount);
		outState.putBoolean("softKeyboardVisibility", mSoftKeyboardVisible);
		outState.putSerializable("consoleEntries", mConsoleEntries);
		outState.putSerializable("commandEntries", mCommandHistoryEntries);
		if (mServer != null) {
			mServer.detach();
		}
		outState.putSerializable("server", mServer);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onRestoreInstanceState(Bundle state) {
		super.onRestoreInstanceState(state);
		mInputEditText.setText(state.getString("input"));
		mInputEditText.setSelection(state.getInt("cursorPos"));
		mInputEditText.requestFocus();
		mEntryCount = state.getInt("entryCount");
		mSoftKeyboardVisible = state.getBoolean("softKeyboardVisibility");

		mConsoleEntries = (ArrayList<ConsoleEntry>) state.getSerializable("consoleEntries");
		mConsoleAdapter = new ConsoleEntryAdapter(this, mConsoleEntries);
		setListAdapter(mConsoleAdapter);
		updateConsoleEntries();

		mCommandHistoryEntries = (ArrayList<String>) state.getSerializable("commandEntries");
		mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistoryEntries);
		mCommandHistoryListView.setAdapter(mCommandHistoryAdapter);
		updateCommandHistoryEntries();

		mServer = (HermitServer) state.getSerializable("server");
		if (mServer != null) {
			mServer.attach(this);
		}
	}

	@Override
	public void onBackPressed() {
		ConsoleExitDialog ced = new ConsoleExitDialog();
		ced.show(getFragmentManager(), "exit");
	}

	@Override
	public void onRestart() {
		/* Prevents HermitServer from throwing a NullPointerException
		 * (since ConsoleActivity doesn't get serialized) */
		if (mServer != null) {
			mServer.attach(this);
		}
		super.onRestart();
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.complete:
			attemptWordCompletion();
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		AdapterContextMenuInfo info = (AdapterContextMenuInfo) menuInfo;
		if (info.position != mConsoleEntries.size() && //To prevent footer from spawning a ContextMenu
				!mConsoleEntries.get(info.position).getContents().isEmpty()) { //To prevent empty lines
			super.onCreateContextMenu(menu, v, menuInfo);

			if (mTempCommand != null) { //If user dragged CommandIcon onto entry
				menu.setHeaderTitle("Execute " + mTempCommand + " on...");
			} else { //If user long-clicked entry
				menu.setHeaderTitle(R.string.context_menu_title);
				menu.add(0, REARRANGE_ID, 0, "Rearrange words");
			}

			int order = 1;
			for (String keyword : mConsoleEntries.get(info.position).getKeywords()) {
				menu.add(0, v.getId(), order, keyword);
				order++;
			}
		}
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		if (item != null) {
			String keywordNStr = item.getTitle().toString();
			if (mTempCommand != null) { //If DragIcon command is run
				if (mInputEnabled) {
					mDispatcher.runOnConsole(mTempCommand, keywordNStr);
				} else {
					mInputEditText.setText(mTempCommand + " " + keywordNStr);
				}
			} else { //If long-click command is run
				if (item.getItemId() == REARRANGE_ID) {
					ConsoleEntry sEntry = mConsoleEntries.get(((AdapterContextMenuInfo) item.getMenuInfo()).position);
					showEntryDialog(sEntry.getNum(), sEntry.getContents(), REARRANGE_TAG);
				} else {
					if (mInputEnabled) {
						mDispatcher.runKeywordCommand(keywordNStr, keywordNStr);
					} else {
						mInputEditText.setText(CommandDispatcher.getKeyword(keywordNStr)
								.getCommand().getCommandName() + " " + keywordNStr);
					}
				}
			}
			mInputEditText.requestFocus(); //Prevents ListView from stealing focus
		}
		mTempCommand = null;
		return super.onContextItemSelected(item);
	}

	@Override
	public void onContextMenuClosed(Menu menu) {
		//Ensures that the temp variables do not persist to next context menu opening
		mTempCommand = null;
	}

	@Override
	public boolean onKeyDown(int keyCode, KeyEvent event) {
		switch (keyCode) {
		case KeyEvent.KEYCODE_TAB:
			attemptWordCompletion();
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	public void addCommandEntry(String commandName) {
		if ((mCommandHistoryEntries.size() == 0
				|| commandName != mCommandHistoryEntries.get(0))
				&& DragIcon.commandHasIcon(this, commandName)) {
			mCommandHistoryEntries.add(0, commandName);
			updateCommandHistoryEntries();
		}
	}

	/**
	 * Adds a new entry to the console.
	 * @param contents The message to be shown in the entry.
	 */
	public void addConsoleEntry(String contents) {
		ConsoleEntry ce = new ConsoleEntry(contents, mEntryCount);
		mConsoleEntries.add(ce);
		updateConsoleEntries();
		mEntryCount++;
		mInputNum.setText("hermit<" + mEntryCount + "> ");
		scrollToBottom();
	}

	/**
	 * Appends a newline and newContents to the most recent entry.
	 * @param newContents The message to be appended to the entry.
	 */
	public void appendConsoleEntry(String newContents) {
		String contents = mConsoleEntries.get(mConsoleEntries.size() - 1).getContents();
		contents += /*"Disabled pretty-printing <br />"*/ "\n" + newContents;
		mConsoleEntries.remove(mConsoleEntries.size() - 1);
		/* Use 1 less than mEntryCount, since we are retroactively modifying an entry after
		 * addEntry() was called (which incremented mEntryCount) */
		ConsoleEntry ce = new ConsoleEntry(contents, mEntryCount - 1);
		mConsoleEntries.add(ce);
		updateConsoleEntries();
		scrollToBottom();
	}

	/**
	 * Appends a progress spinner below the most recent entry's contents. Ideal for
	 * doing asynchronous tasks (such as HermitServer requests). The loading bar
	 * will be destroyed when appendConsoleEntry() is called.
	 */
	public void appendProgressSpinner() {
		String contents = mConsoleEntries.get(mConsoleEntries.size() - 1).getContents();
		mConsoleEntries.remove(mConsoleEntries.size() - 1);
		ConsoleEntry ce = new ConsoleEntry(contents, mEntryCount - 1, true);
		mConsoleEntries.add(ce);
		updateConsoleEntries();
		scrollToBottom();
	}

	/**
	 * Removes all console entries and resets the entry count.
	 */
	public void clear() {
		mConsoleEntries.clear();
		mEntryCount = 0;
		updateConsoleEntries();
	}

	public void disableInput() {
		mInputEnabled = false;
	}

	public void enableInput() {
		mInputEnabled = true;
	}

	/**
	 * Exits the console activity.
	 */
	public void exit() {
		if (mServer != null) {
			mServer.cancel(true);
		}
		finish();
	}

	public SlidingMenu getSlidingMenu() {
		return mSlidingMenu;
	}

	public void setInputText(String text) {
		mInputEditText.setText(text);
		mInputEditText.setSelection(mInputEditText.getText().length());
	}

	public void setServer(HermitServer server) {
		mServer = server;
	}

	/**
	 * Sets the name of the Command to be run on a keyword when selected from a ContextMenu.
	 * Intended to be used in conjunction with CommandIcon.
	 * @param commandName The name of the Command that will be run (if selected).
	 */
	public void setTempCommand(String commandName) {
		mTempCommand = commandName;
	}

	public void showEntryDialog(int entryNum, String entryContents, String tag) {
		FragmentTransaction ft = getFragmentManager().beginTransaction();
		Fragment prev = getFragmentManager().findFragmentByTag("selecDialog");
		if (prev != null) {
			ft.remove(prev);
		}
		ft.addToBackStack(null);
		DialogFragment newFrag = null;
		if (tag == SELECTION_TAG) {
			newFrag = ConsoleEntrySelectionDialog.newInstance(entryNum, entryContents);
		} else if (tag == REARRANGE_TAG) {
			newFrag = ConsoleEntryRearrangeDialog.newInstance(entryNum, entryContents);
		} else if (tag == KEYWORD_SWAP_TAG) {
			newFrag = KeywordSwapDialog.newInstance(entryNum, entryContents);
		} else if (tag == WORD_COMPLETION_TAG) {
			newFrag = WordCompletionDialog.newInstance(mCompleter);
		}
		ft.add(newFrag, tag);
		ft.commit();
	}

	private void loadExpandableMenuData() {
		TypedArray ta = getResources().obtainTypedArray(R.array.command_group_arrays);
		for (int i = 0; i < ta.length(); i++) {
			String[] parents = getResources().getStringArray(R.array.command_groups);
			int id = ta.getResourceId(i, 0);
			if (id > 0) {
				String[] children = getResources().getStringArray(id);
				for (int j = 1; j < children.length; j++) { //Don't include id
					if (CommandDispatcher.isAlias(children[j])) {
						addCommandToExpandableMenu(parents[i], CommandDispatcher.unaliasCommand(children[j]));
					} else {
						addCommandToExpandableMenu(parents[i], children[j]);
					}
				}
			}
		}

		ta.recycle();
	}

	private int addCommandToExpandableMenu(String groupName, String commandName) {
		int groupPosition = 0;

		//check the hash map if the group already exists
		ArrayList<String> commandNames = mCommandExpandableGroupMap.get(groupName); 
		//add the group if doesn't exists
		if(commandNames == null) {
			commandNames = new ArrayList<String>();
			mCommandExpandableGroupMap.put(groupName, commandNames);
			mCommandExpandableGroups.add(groupName);
		}
		commandNames.add(commandName);

		//find the group position inside the list
		groupPosition = mCommandExpandableGroups.indexOf(groupName);
		return groupPosition;
	}

	private void attemptWordCompletion() {
		String input = mInputEditText.getText().toString().trim();
		if (input.split(WHITESPACE).length <= 1) {
			String completion = mCompleter.completeWord(input);
			if (completion != null) {
				setInputText(completion);
			}
		}
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
		mConsoleListView.post(new Runnable() {
			public void run() {
				setSelection(mConsoleListView.getCount());
				mConsoleListView.smoothScrollToPosition(mConsoleListView.getCount());
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

	/**
	 * Refreshes the console entries, removing excessive entries from the top if ENTRY_LIMIT is exceeded.
	 */
	private void updateConsoleEntries() {
		if (mConsoleEntries.size() > ENTRY_CONSOLE_LIMIT) {
			mConsoleEntries.remove(0);
		}
		mConsoleAdapter.notifyDataSetChanged();
		mInputNum.setText("hermit<" + mEntryCount + "> ");
	}

	private void updateCommandHistoryEntries() {
		if (mCommandHistoryEntries.size() > ENTRY_COMMAND_HISTORY_LIMIT) {
			mCommandHistoryEntries.remove(0);
		}
		mCommandHistoryAdapter.notifyDataSetChanged();
	}

}