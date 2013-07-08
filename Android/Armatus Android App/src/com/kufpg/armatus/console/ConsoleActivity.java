package com.kufpg.armatus.console;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.google.common.collect.ListMultimap;
import com.kufpg.armatus.EditManager.Edit;
import com.kufpg.armatus.R;
import com.kufpg.armatus.BaseActivity;
import com.kufpg.armatus.console.ConsoleEdits.ConsoleClearer;
import com.kufpg.armatus.console.ConsoleEdits.ConsoleEntryAdder;
import com.kufpg.armatus.dialog.ConsoleEntrySelectionDialog;
import com.kufpg.armatus.dialog.GestureDialog;
import com.kufpg.armatus.dialog.KeywordSwapDialog;
import com.kufpg.armatus.dialog.WordCompletionDialog;
import com.kufpg.armatus.dialog.YesOrNoDialog;
import com.kufpg.armatus.drag.DragIcon;
import com.kufpg.armatus.util.JsonUtils;
import com.slidingmenu.lib.SlidingMenu;

import android.app.DialogFragment;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.Context;
import android.content.DialogInterface;
import android.content.res.Configuration;
import android.graphics.Typeface;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.ActionMode;
import android.view.ContextMenu;
import android.view.DragEvent;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnDragListener;
import android.view.View.OnLongClickListener;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.View.OnKeyListener;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.view.inputmethod.InputMethodManager;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.EditText;
import android.widget.ExpandableListView;
import android.widget.Filter.FilterListener;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.TextView.OnEditorActionListener;

/**
 * Activity that displays an interactive, feature-rich
 * (at least it will be some day) HERMIT console.
 */
public class ConsoleActivity extends BaseActivity {

	public static final String DRAG_LAYOUT = "drag_layout";
	public static final int DEFAULT_FONT_SIZE = 15;
	public static final int ENTRY_CONSOLE_LIMIT = 100;
	public static final int ENTRY_COMMAND_HISTORY_LIMIT = 200;
	public static final String SELECTION_TAG = "selection";
	public static final String KEYWORD_SWAP_TAG = "keywordswap";
	public static final String WORD_COMPLETION_TAG = "wordcomplete";
	public static final String CONSOLE_TAG = "console";
	public static final String COMMANDS_TAG = "commands";
	public static final String SESSION_HISTORY_FILENAME = "/history.txt";
	public static final String UNDO_HISTORY_FILENAME = "/undo.txt";
	public static Typeface TYPEFACE;
	private static final String TYPEFACE_PATH = "fonts/DroidSansMonoDotted.ttf";
	private static final int TIME_PER_ENTRY = 200;

	private ConsoleListView mConsoleListView;
	private ListView mCommandHistoryListView;
	private ExpandableListView mCommandExpandableMenuView;
	private ConsoleEntryAdapter mConsoleAdapter;
	private CommandHistoryAdapter mCommandHistoryAdapter;
	private CommandExpandableMenuAdapter mCommandExpandableMenuAdapter;
	private List<ConsoleEntry> mFilteredConsoleEntries = new ArrayList<ConsoleEntry>();
	private List<ConsoleEntry> mOriginalConsoleEntries = new ArrayList<ConsoleEntry>();
	private List<String> mCommandHistoryEntries = new ArrayList<String>();
	private View mInputView, mRootView, mBackground, mListTop, mListBottom;
	private TextView mConsoleInputNum;
	private EditText mConsoleInput, mFilterInput;
	private SlidingMenu mSlidingMenu;
	private CommandDispatcher mDispatcher;
	private WordCompleter mCompleter;
	private ActionMode mActionMode;
	private ConsoleEntryCallback mCallback;
	private String mTempCommand, mTempFilterInput;
	private JSONObject mHistory;
	private boolean mInputEnabled = true;
	private boolean mSoftKeyboardVisible = true;
	private boolean mFindTextEnabled = false;
	private boolean mTempIsFiltering = false;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console_activity);

		TYPEFACE = Typeface.createFromAsset(getAssets(), TYPEFACE_PATH);

		//Ensures soft keyboard remains open
		setSoftKeyboardVisible(true);
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

		mBackground = (View) findViewById(R.id.console_empty_space);
		mBackground.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				if (!mSoftKeyboardVisible) {
					((InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE))
					.toggleSoftInput(InputMethodManager.SHOW_FORCED,0);
				}
			}
		});
		mBackground.setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				mConsoleInput.performLongClick();
				return false;
			}
		});

		mDispatcher = new CommandDispatcher(this);
		mConsoleListView = (ConsoleListView) findViewById(R.id.console_list_view);
		mConsoleAdapter = new ConsoleEntryAdapter(this, mFilteredConsoleEntries);
		//TODO: Make mListView scroll when CommandIcons are dragged near boundaries
		registerForContextMenu(mConsoleListView);
		mInputView = getLayoutInflater().inflate(R.layout.console_input, null);
		mConsoleInputNum = (TextView) mInputView.findViewById(R.id.test_code_input_num);
		mConsoleInput = (EditText) mInputView.findViewById(R.id.test_code_input_edit_text);
		mConsoleInput.addTextChangedListener(new TextWatcher() {
			@Override
			public void afterTextChanged(Editable s) {}
			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {}
			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
				//Closes SlidingMenu and scroll to bottom if user begins typing
				mSlidingMenu.showContent();
				setContextualActionBarVisible(false);
				String input = mConsoleInput.getText().toString().trim();
				if (input.split(WHITESPACE).length <= 1) {
					mCompleter.filterDictionary(input);
				}
				mConsoleListView.post(new Runnable() {
					public void run() {
						//DON'T use scrollToBottom(); it will cause a strange jaggedy scroll effect
						mConsoleListView.setSelection(mConsoleListView.getCount() - 1);
					}
				});
			}
		});
		//Processes user input (and runs command, if input is a command) when Enter is pressed
		mConsoleInput.setOnKeyListener(new OnKeyListener() {
			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP
						&& mInputEnabled) {
					String[] inputs = mConsoleInput.getText().
							toString().trim().split(WHITESPACE);
					if (CommandDispatcher.isCommand(inputs[0])) {
						if (inputs.length == 1) {
							mDispatcher.runOnConsole(inputs[0]);
						} else {
							mDispatcher.runOnConsole(inputs[0], Arrays.copyOfRange
									(inputs, 1, inputs.length));
						}
					} else {
						addConsoleEntry(mConsoleInput.getText().toString());
					}
					mConsoleInput.setText("");
					return true;
				} else if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP
						&& !mInputEnabled) {
					mConsoleInput.setText("");
				}
				return false;
			}
		});
		mConsoleInput.requestFocus();
		mConsoleListView.addFooterView(mInputView, null, false);
		mConsoleListView.setAdapter(mConsoleAdapter); //MUST be called after addFooterView()
		updateConsoleEntries();

		mListTop = (View) findViewById(R.id.console_list_view_top);
		mListBottom = (View) findViewById(R.id.console_list_view_bottom);
		//		mListTop.setOnDragListener(new DragSinkListener() {
		//			@Override
		//			public void onDragDropped(View dragSource, View dragSink, DragEvent event) {
		//				mConsoleListView.stopScroll();
		//			}
		//
		//			@Override
		//			public void onDragEnded(View dragSource, View dragSink, DragEvent event) {
		//				mConsoleListView.stopScroll();
		//			}
		//
		//			@Override
		//			public void onDragEntered(View dragSource, View dragSink, DragEvent event) {
		//				int viewsAbove = mConsoleListView.getFirstVisiblePosition();
		//				mConsoleListView.smoothScrollToPositionFromTop(0, 0, viewsAbove * TIME_PER_ENTRY);
		//			}
		//
		//			@Override
		//			public void onDragExited(View dragSource, View dragSink, DragEvent event) {
		//				mConsoleListView.stopScroll();
		//			}
		//		});
		mListTop.setOnDragListener(new OnListEdgeDragListener() {
			@Override
			public boolean onDrag(View v, DragEvent event) {
				switch (event.getAction()) {
				case DragEvent.ACTION_DRAG_ENTERED:
					int viewsAbove = mConsoleListView.getFirstVisiblePosition();
					mConsoleListView.smoothScrollToPositionFromTop(0, 0, viewsAbove * TIME_PER_ENTRY);
					return true;
				default:
					return super.onDrag(v, event);
				}
			}

		});
		mListBottom.setOnDragListener(new OnListEdgeDragListener() {
			@Override
			public boolean onDrag(View v, DragEvent event) {
				switch (event.getAction()) {
				case DragEvent.ACTION_DRAG_ENTERED:
					int totalViews = mFilteredConsoleEntries.size();
					int viewsBelow = totalViews - mConsoleListView.getLastVisiblePosition();
					mConsoleListView.smoothScrollToPositionFromTop(totalViews, 0, viewsBelow * TIME_PER_ENTRY);
					return true;
				default:
					return super.onDrag(v, event);
				}
			}

		});
		//		mListBottom.setOnDragListener(new DragSinkListener() {
		//			@Override
		//			public void onDragDropped(View dragSource, View dragSink, DragEvent event) {
		//				mConsoleListView.stopScroll();
		//			}
		//
		//			@Override
		//			public void onDragEnded(View dragSource, View dragSink, DragEvent event) {
		//				mConsoleListView.stopScroll();
		//			}
		//
		//			@Override
		//			public void onDragEntered(View dragSource, View dragSink, DragEvent event) {
		//				int totalViews = mFilteredConsoleEntries.size();
		//				int viewsBelow = totalViews - mConsoleListView.getLastVisiblePosition();
		//				mConsoleListView.smoothScrollToPositionFromTop(totalViews, 0, viewsBelow * TIME_PER_ENTRY);
		//			}
		//
		//			@Override
		//			public void onDragExited(View dragSource, View dragSink, DragEvent event) {
		//				mConsoleListView.stopScroll();
		//			}
		//		});

		mConsoleInputNum.setTypeface(TYPEFACE);
		mConsoleInput.setTypeface(TYPEFACE);

		mSlidingMenu = (SlidingMenu) findViewById(R.id.console_sliding_menu);
		refreshSlidingMenu();

		mCommandHistoryListView = (ListView)findViewById(R.id.History);
		mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistoryEntries);
		mCommandHistoryListView.setAdapter(mCommandHistoryAdapter);
		updateCommandHistoryEntries();

		List<String> expandableGroupList = CommandExpandableMenuFactory.getGroupList(this);
		ListMultimap<String, String> expandableGroupMap = CommandExpandableMenuFactory.getGroupMap(this);
		mCompleter = new WordCompleter(this, expandableGroupMap.values());

		mCommandExpandableMenuView = (ExpandableListView) findViewById(R.id.command_expandable_menu);
		mCommandExpandableMenuAdapter = new CommandExpandableMenuAdapter
				(this, expandableGroupList, expandableGroupMap);
		mCommandExpandableMenuView.setAdapter(mCommandExpandableMenuAdapter);
		//		mCommandExpandableMenuView.setOnDragListener(new DragSinkListener() {
		//			@Override
		//			public void onDragStarted(View dragSource, View dragSink, DragEvent event) {
		//				getSlidingMenu().showContent();
		//			}
		//
		//			@Override
		//			public void onDragDropped(View dragSource, View dragSink, DragEvent event) {
		//				dragSource.setVisibility(View.VISIBLE);
		//			}
		//
		//			@Override
		//			public void onDragEnded(View dragSource, View dragSink, DragEvent event) {
		//				dragSource.setVisibility(View.VISIBLE);
		//			}	
		//		});
		mCommandExpandableMenuView.setOnDragListener(new OnDragListener() {
			@Override
			public boolean onDrag(View v, DragEvent event) {
				switch (event.getAction()) {
				case DragEvent.ACTION_DRAG_STARTED:
					getSlidingMenu().showContent();
					return true;
				case DragEvent.ACTION_DROP:
				case DragEvent.ACTION_DRAG_ENDED:
					View dragSource = (View) event.getLocalState();
					dragSource.setVisibility(View.VISIBLE);
					return true;
				default:
					return false;
				}
			}
		});

		mCallback = new ConsoleEntryCallback(this);
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putString("consoleInput", mConsoleInput.getText().toString());
		outState.putInt("consoleInputCursor", mConsoleInput.getSelectionStart());
		if (mFilterInput != null) {
			outState.putString("filterInput", mFilterInput.getText().toString());
		}
		outState.putBoolean("softKeyboardVisibility", mSoftKeyboardVisible);
		outState.putBoolean("findTextEnabled", mFindTextEnabled);
		outState.putBoolean("isFiltering", mConsoleAdapter.isFiltering());
		outState.putSerializable("filteredConsoleEntries", (Serializable) mFilteredConsoleEntries);
		outState.putSerializable("originalConsoleEntries", (Serializable) mOriginalConsoleEntries);
		outState.putSerializable("commandEntries", (Serializable) mCommandHistoryEntries);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onRestoreInstanceState(Bundle state) {
		super.onRestoreInstanceState(state);
		mConsoleInput.setText(state.getString("consoleInput"));
		mSoftKeyboardVisible = state.getBoolean("softKeyboardVisibility");
		setSoftKeyboardVisible(mSoftKeyboardVisible);
		mFindTextEnabled = state.getBoolean("findTextEnabled");
		if (mFindTextEnabled) {
			mTempFilterInput = state.getString("filterInput");
			mTempIsFiltering = state.getBoolean("isFiltering");
		} else {
			mConsoleInput.setSelection(state.getInt("consoleCursorPos"));
			mConsoleInput.requestFocus();
		}

		mFilteredConsoleEntries = (List<ConsoleEntry>) state.getSerializable("filteredConsoleEntries");
		mOriginalConsoleEntries = (List<ConsoleEntry>) state.getSerializable("originalConsoleEntries");
		mConsoleAdapter = new ConsoleEntryAdapter(this, mFilteredConsoleEntries, mOriginalConsoleEntries);
		mConsoleListView.setAdapter(mConsoleAdapter);
		updateConsoleEntries();

		mCommandHistoryEntries = (List<String>) state.getSerializable("commandEntries");
		mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistoryEntries);
		mCommandHistoryListView.setAdapter(mCommandHistoryAdapter);
		updateCommandHistoryEntries();

		for (Edit edit : getEditManager()) {
			if (edit instanceof ConsoleEdit) {
				((ConsoleEdit) edit).attachConsole(this);
			}
		}
	}

	@Override
	public void onBackPressed() {
		String title = getResources().getString(R.string.console_exit_title);
		String message = getResources().getString(R.string.console_exit_message);
		YesOrNoDialog exitDialog = new YesOrNoDialog(title, message) {
			@Override
			protected void yes(DialogInterface dialog, int whichButton) {
				getEditManager().discardAllEdits();
				finish();
			}
		};
		exitDialog.show(getFragmentManager(), "exit");
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);

		menu.setGroupVisible(R.id.menu_icons_group, !mFindTextEnabled);
		menu.setGroupVisible(R.id.history_group, true);
		menu.findItem(R.id.find_text_option).setVisible(!mFindTextEnabled);
		menu.setGroupVisible(R.id.find_text_group, mFindTextEnabled);

		if (mFindTextEnabled) {
			View actionView = menu.findItem(R.id.find_text_action).getActionView();
			mFilterInput = (EditText) actionView.findViewById(R.id.find_text_box);
			final TextView filterMatches = (TextView) actionView.findViewById(R.id.find_text_matches);
			final FilterListener listener = new FilterListener() {
				@Override
				public void onFilterComplete(int count) {
					String caption = mConsoleAdapter.getFilterMatches() + " match";
					filterMatches.setText(caption + (caption.startsWith("1") ? "" : "es"));
				}
			};

			if (mTempFilterInput != null) {
				mFilterInput.setText(mTempFilterInput);
				mFilterInput.setSelection(mFilterInput.length());
				if (mTempIsFiltering) {
					mTempIsFiltering = false;
					mConsoleAdapter.getFilter().filter(mTempFilterInput, listener);
				}
			}

			mFilterInput.requestFocus();
			mFilterInput.setOnEditorActionListener(new OnEditorActionListener() {
				@Override
				public boolean onEditorAction(TextView v, int actionId,
						KeyEvent event) {
					if (event == null || event.getAction() == KeyEvent.ACTION_UP) {
						CharSequence contents = mFilterInput.getText();
						mConsoleAdapter.getFilter().filter(contents, listener);
						return true;
					}
					return false;
				}
			});
			mFilterInput.setTypeface(TYPEFACE);
		}

		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.gestures:
			GestureDialog gd = new GestureDialog();
			gd.show(getFragmentManager(), "gesture");
			return true;
		case R.id.complete:
			attemptWordCompletion();
			return true;
		case R.id.find_text_option:
			mFindTextEnabled = true;
			invalidateOptionsMenu();
			return true;
		case R.id.find_text_cancel:
			mFindTextEnabled = false;
			mConsoleAdapter.getFilter().filter(null);
			mConsoleInput.requestFocus();
			invalidateOptionsMenu();
			return true;
		case R.id.save_history:
			if (mInputEnabled) {
				try {
					JSONArray consoleHistory = new JSONArray();
					for (ConsoleEntry entry : mOriginalConsoleEntries) {
						JSONObject entryJson = new JSONObject();
						entryJson.put("num", entry.getNum());
						entryJson.put("contents", entry.getContents());
						consoleHistory.put(entryJson);
					}

					JSONArray commandHistory = new JSONArray();
					for (String command : mCommandHistoryEntries) {
						commandHistory.put(command);
					}

					mHistory = new JSONObject();
					mHistory.put(CONSOLE_TAG, consoleHistory);
					mHistory.put(COMMANDS_TAG, commandHistory);

					String path = "";
					if (getPrefs().getBoolean(HISTORY_USE_CACHE_KEY, true)) {
						path = getPrefs().getString(HISTORY_DIR_KEY, null);
					} else {
						path = CACHE_DIR;
					}
					final File file = new File(path + SESSION_HISTORY_FILENAME);
					if (file.exists()) {
						JsonUtils.saveJsonFile(mHistory, file.getAbsolutePath());
						showToast("Save complete!");
					} else {
						showToast("Error: file not found");
					}
				} catch (JSONException e) {
					e.printStackTrace();
				}
			}
			return true;
		case R.id.load_history:
			if (mInputEnabled) {
				String path = "";
				if (getPrefs().getBoolean(HISTORY_USE_CACHE_KEY, true)) {
					path = getPrefs().getString(HISTORY_DIR_KEY, null);
				} else {
					path = CACHE_DIR;
				}

				final File file = new File (path + SESSION_HISTORY_FILENAME);
				if (file.exists()) {
					JSONObject history = null;
					try {
						history = JsonUtils.openJsonFile(file.getAbsolutePath());

						JSONArray consoleHistory = history.getJSONArray(CONSOLE_TAG);
						mFilteredConsoleEntries.clear();
						for (int i = 0; i < consoleHistory.length(); i++) {
							JSONObject jsonEntry = consoleHistory.getJSONObject(i);
							int num = jsonEntry.getInt("num");
							String contents = jsonEntry.getString("contents");
							ConsoleEntry entry = new ConsoleEntry(num, contents);
							mFilteredConsoleEntries.add(entry);
						}
						mOriginalConsoleEntries = new ArrayList<ConsoleEntry>(mFilteredConsoleEntries);
						mConsoleAdapter = new ConsoleEntryAdapter(this, mFilteredConsoleEntries);
						mConsoleListView.setAdapter(mConsoleAdapter);
						updateConsoleEntries();

						JSONArray commandHistory = history.getJSONArray(COMMANDS_TAG);
						mCommandHistoryEntries.clear();
						for (int i = 0; i < commandHistory.length(); i++) {
							mCommandHistoryEntries.add(commandHistory.getString(i));
						}
						mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistoryEntries);
						mCommandHistoryListView.setAdapter(mCommandHistoryAdapter);
						updateCommandHistoryEntries();

						mConsoleInput.requestFocus();
						showToast("Loading complete!");
					} catch (JSONException e) {
						showToast("Error: invalid JSON");
					} catch (FileNotFoundException e) {
						showToast("Error: file not found"); //Should never happen
					}
				} else {
					showToast("Error: file not found");
				}
			}
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		AdapterContextMenuInfo info = (AdapterContextMenuInfo) menuInfo;
		if (info.position != mFilteredConsoleEntries.size() && //To prevent footer from spawning a ContextMenu
				!mFilteredConsoleEntries.get(info.position).getContents().isEmpty()) { //To prevent empty lines
			super.onCreateContextMenu(menu, v, menuInfo);

			if (mTempCommand != null) { //If user dragged CommandIcon onto entry
				menu.setHeaderTitle("Execute " + mTempCommand + " on...");
			} else { //If user long-clicked entry
				menu.setHeaderTitle(R.string.context_menu_title);
				menu.add(0, 42, 0, "Sample transformation (does nothing)");
			}

			int order = 1;
			for (String keyword : mFilteredConsoleEntries.get(info.position).getKeywords()) {
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
					mConsoleInput.setText(mTempCommand + " " + keywordNStr);
				}
			} else { //If long-click command is run
				if (mInputEnabled) {
					mDispatcher.runKeywordCommand(keywordNStr, keywordNStr);
				} else {
					mConsoleInput.setText(CommandDispatcher.getKeyword(keywordNStr)
							.getCommand().getCommandName() + " " + keywordNStr);
				}
			}
			mConsoleInput.requestFocus(); //Prevents ListView from stealing focus
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

	@Override
	public boolean canRedo() {
		return super.canRedo() && mInputEnabled;
	}

	@Override
	public boolean canUndo() {
		return super.canUndo() && mInputEnabled;
	}

	public void addCommandEntry(String commandName) {
		if ((mCommandHistoryEntries.size() == 0
				|| commandName != mCommandHistoryEntries.get(0))
				&& DragIcon.commandHasIcon(this, commandName)) {
			mCommandHistoryEntries.add(0, commandName);
			updateCommandHistoryEntries();
		}
	}

	public void addConsoleEntry(String contents) {
		ConsoleEntryAdder edit = new ConsoleEntryAdder(this, contents);
		getEditManager().applyEdit(edit);
	}

	void addConsoleEntry(ConsoleEntry entry) {
		mFilteredConsoleEntries.add(entry);
		mOriginalConsoleEntries.add(new ConsoleEntry(entry));
		updateConsoleEntries();
		scrollToBottom();
	}

	void removeConsoleEntry() {
		if (!mFilteredConsoleEntries.isEmpty()) {
			mFilteredConsoleEntries.remove(mFilteredConsoleEntries.size() - 1);
			mOriginalConsoleEntries.remove(getEntryCount() - 1);
			updateConsoleEntries();
			scrollToBottom();
		}
	}

	public void appendConsoleEntry(String newContents) {
		mFilteredConsoleEntries.get(mFilteredConsoleEntries.size() - 1).appendContents(newContents);
		mOriginalConsoleEntries.get(getEntryCount() - 1).appendContents(newContents);
		updateConsoleEntries();
		scrollToBottom();
	}

	public void clear() {
		ConsoleClearer clear = new ConsoleClearer(this, mOriginalConsoleEntries);
		getEditManager().applyEdit(clear);
	}

	/**
	 * Removes all console entries and resets the entry count.
	 */
	void clearConsole() {
		mFilteredConsoleEntries.clear();
		mOriginalConsoleEntries.clear();
		updateConsoleEntries();
	}

	public void disableInput() {
		mInputEnabled = false;
	}

	public void enableInput() {
		mInputEnabled = true;
	}

	public int getEntryCount() {
		return mOriginalConsoleEntries.size();
	}

	public ListView getListView() {
		return mConsoleListView;
	}

	public SlidingMenu getSlidingMenu() {
		return mSlidingMenu;
	}

	public void setInputEnabled(boolean enabled) {
		mInputEnabled = enabled;
	}

	public void setInputText(String text) {
		mConsoleInput.setText(text);
		mConsoleInput.setSelection(mConsoleInput.getText().length());
	}

	/**
	 * Sets the name of the Command to be run on a keyword when selected from a ContextMenu.
	 * Intended to be used in conjunction with CommandIcon.
	 * @param commandName The name of the Command that will be run (if selected).
	 */
	public void setTempCommand(String commandName) {
		mTempCommand = commandName;
	}

	public void setContextualActionBarVisible(boolean visible) {
		if (visible && !mCallback.isVisible()) {
			mActionMode = startActionMode(mCallback);
		} else if (!visible && mCallback.isVisible()) {
			mActionMode.finish();
		}
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
		} else if (tag == KEYWORD_SWAP_TAG) {
			newFrag = KeywordSwapDialog.newInstance(entryNum, entryContents);
		} else if (tag == WORD_COMPLETION_TAG) {
			newFrag = WordCompletionDialog.newInstance(mCompleter);
		}
		ft.add(newFrag, tag);
		ft.commit();
	}

	/**
	 * Appends a progress spinner below the most recent entry's contents. Ideal for
	 * doing asynchronous tasks (such as HermitServer requests).
	 */
	public void updateProgressSpinner(boolean shown) {
		mFilteredConsoleEntries.get(mFilteredConsoleEntries.size() - 1).setWaiting(shown);
		updateConsoleEntries();
		scrollToBottom();
	}

	private void attemptWordCompletion() {
		String input = mConsoleInput.getText().toString().trim();
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
				mConsoleListView.setSelection(mConsoleListView.getCount());
				mConsoleListView.smoothScrollToPosition(mConsoleListView.getCount());
			}
		});
	}

	/**
	 * Shows or hides the soft keyboard.
	 * @param visible Set to true to show soft keyboard, false to hide.
	 */
	public void setSoftKeyboardVisible(boolean visible) {
		if (visible) {
			getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);
		} else {
			getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_HIDDEN);
		}
	}

	/**
	 * Refreshes the console entries, removing excessive entries from the top if ENTRY_LIMIT is exceeded.
	 */
	private void updateConsoleEntries() {
		if (mFilteredConsoleEntries.size() > ENTRY_CONSOLE_LIMIT) {
			mFilteredConsoleEntries.remove(0);
			mOriginalConsoleEntries.remove(0);
		}
		mConsoleAdapter.notifyDataSetChanged();
		updateEntryCount();
	}

	void updateConsoleEntries(List<ConsoleEntry> newEntries) {
		mFilteredConsoleEntries.clear();
		mFilteredConsoleEntries.addAll(newEntries);
		mOriginalConsoleEntries = new ArrayList<ConsoleEntry>(mFilteredConsoleEntries);
		mConsoleAdapter.notifyDataSetChanged();
		updateEntryCount();
	}

	private void updateCommandHistoryEntries() {
		if (mCommandHistoryEntries.size() > ENTRY_COMMAND_HISTORY_LIMIT) {
			mCommandHistoryEntries.remove(0);
		}
		mCommandHistoryAdapter.notifyDataSetChanged();
	}

	private void updateEntryCount() {
		mConsoleInputNum.setText("hermit<" + getEntryCount() + "> ");
	}

	private class OnListEdgeDragListener implements OnDragListener {
		@Override
		public boolean onDrag(View v, DragEvent event) {
			switch (event.getAction()) {
			case DragEvent.ACTION_DROP:
			case DragEvent.ACTION_DRAG_ENDED:
			case DragEvent.ACTION_DRAG_EXITED:
				mConsoleListView.stopScroll();
				break;
			}
			return true;
		}
	}

}