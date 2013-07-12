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
import com.kufpg.armatus.MainActivity;
import com.kufpg.armatus.R;
import com.kufpg.armatus.BaseActivity;
import com.kufpg.armatus.console.ConsoleEdits.ConsoleClearer;
import com.kufpg.armatus.console.ConsoleEdits.ConsoleEntryAdder;
import com.kufpg.armatus.console.ConsoleSearcher.Direction;
import com.kufpg.armatus.console.ConsoleSearcher.MatchParams;
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
import android.content.Intent;
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
	private static final int SCROLL_DURATION = 500;
	public static final String SELECTION_TAG = "selection";
	public static final String KEYWORD_SWAP_TAG = "keywordswap";
	public static final String WORD_COMPLETION_TAG = "wordcomplete";
	public static final String CONSOLE_TAG = "console";
	public static final String COMMANDS_TAG = "commands";
	public static final String SESSION_HISTORY_FILENAME = "/history.txt";
	public static final String UNDO_HISTORY_FILENAME = "/undo.txt";
	public static Typeface TYPEFACE;
	private static final String TYPEFACE_PATH = "fonts/DroidSansMonoDotted.ttf";

	private ConsoleListView mConsoleListView;
	private ListView mCommandHistoryListView;
	private ExpandableListView mCommandExpandableMenuView;
	private ConsoleEntryAdapter mConsoleAdapter;
	private ConsoleSearcher mSearcher;
	private CommandHistoryAdapter mCommandHistoryAdapter;
	private CommandExpandableMenuAdapter mCommandExpandableMenuAdapter;
	private List<ConsoleEntry> mConsoleEntries = new ArrayList<ConsoleEntry>();
	private List<String> mCommandHistoryEntries = new ArrayList<String>();
	private View mInputView, mRootView, mBackground;
	private TextView mConsoleInputNum, mFilterMatches;
	private EditText mConsoleInputView, mSearchInputView;
	private SlidingMenu mSlidingMenu;
	private CommandDispatcher mDispatcher;
	private WordCompleter mCompleter;
	private ActionMode mActionMode;
	private ConsoleEntryCallback mCallback;
	private String mTempCommand, mTempSearchInput, mPrevSearchCriterion;
	private JSONObject mHistory;
	private boolean mInputEnabled = true;
	private boolean mSoftKeyboardVisible = true;
	private boolean mSearchEnabled = false;

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
				mConsoleInputView.performLongClick();
				return false;
			}
		});

		mDispatcher = new CommandDispatcher(this);
		mConsoleListView = (ConsoleListView) findViewById(R.id.console_list_view);
		mConsoleAdapter = new ConsoleEntryAdapter(this, mConsoleEntries);
		registerForContextMenu(mConsoleListView);
		mInputView = getLayoutInflater().inflate(R.layout.console_input, null);
		mConsoleInputNum = (TextView) mInputView.findViewById(R.id.test_code_input_num);
		mConsoleInputView = (EditText) mInputView.findViewById(R.id.test_code_input_edit_text);
		mConsoleInputView.addTextChangedListener(new TextWatcher() {
			@Override
			public void afterTextChanged(Editable s) {}
			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {}
			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
				//Closes SlidingMenu and scroll to bottom if user begins typing
				mSlidingMenu.showContent();
				setContextualActionBarVisible(false);
				if (mSearchEnabled) {
					executeSearch(null, SearchAction.END, null);
				}
				String input = mConsoleInputView.getText().toString().trim();
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
		mConsoleInputView.setOnKeyListener(new OnKeyListener() {
			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP
						&& mInputEnabled) {
					String[] inputs = mConsoleInputView.getText().
							toString().trim().split(WHITESPACE);
					if (CommandDispatcher.isCommand(inputs[0])) {
						if (inputs.length == 1) {
							mDispatcher.runOnConsole(inputs[0]);
						} else {
							mDispatcher.runOnConsole(inputs[0], Arrays.copyOfRange
									(inputs, 1, inputs.length));
						}
					} else {
						addConsoleEntry(mConsoleInputView.getText().toString());
					}
					mConsoleInputView.setText("");
					return true;
				} else if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP
						&& !mInputEnabled) {
					mConsoleInputView.setText("");
				}
				return false;
			}
		});
		mConsoleInputView.requestFocus();
		mConsoleListView.addFooterView(mInputView, null, false);
		mConsoleListView.setAdapter(mConsoleAdapter); //MUST be called after addFooterView()
		updateConsoleEntries();
		mSearcher = new ConsoleSearcher(mConsoleAdapter);

		mConsoleInputNum.setTypeface(TYPEFACE);
		mConsoleInputView.setTypeface(TYPEFACE);

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
		outState.putString("consoleInput", mConsoleInputView.getText().toString());
		outState.putParcelable("consoleSearcher", mSearcher);
		outState.putInt("consoleInputCursor", mConsoleInputView.getSelectionStart());
		if (mSearchInputView != null) {
			outState.putString("searchInput", mSearchInputView.getText().toString());
			outState.putString("prevSearchCriterion", mPrevSearchCriterion);
		}
		outState.putBoolean("softKeyboardVisibility", mSoftKeyboardVisible);
		outState.putBoolean("findTextEnabled", mSearchEnabled);
		outState.putSerializable("consoleEntries", (Serializable) mConsoleEntries);
		outState.putSerializable("commandEntries", (Serializable) mCommandHistoryEntries);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected void onRestoreInstanceState(Bundle state) {
		super.onRestoreInstanceState(state);
		mConsoleInputView.setText(state.getString("consoleInput"));
		mSoftKeyboardVisible = state.getBoolean("softKeyboardVisibility");
		setSoftKeyboardVisible(mSoftKeyboardVisible);
		mSearchEnabled = state.getBoolean("findTextEnabled");
		if (mSearchEnabled) {
			mTempSearchInput = state.getString("searchInput");
			mPrevSearchCriterion = state.getString("prevSearchCriterion");
		} else {
			mConsoleInputView.setSelection(state.getInt("consoleCursorPos"));
			mConsoleInputView.requestFocus();
		}

		mConsoleEntries = (List<ConsoleEntry>) state.getSerializable("consoleEntries");
		mConsoleAdapter = new ConsoleEntryAdapter(this, mConsoleEntries);
		mConsoleListView.setAdapter(mConsoleAdapter);
		mSearcher = state.getParcelable("consoleSearcher");
		mSearcher.attachAdapter(mConsoleAdapter);
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
				Intent intent = new Intent(ConsoleActivity.this, MainActivity.class);
				intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
				startActivity(intent);
			}
		};
		exitDialog.show(getFragmentManager(), "exit");
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);

		menu.setGroupVisible(R.id.menu_icons_group, !mSearchEnabled);
		menu.setGroupVisible(R.id.history_group, true);
		menu.findItem(R.id.find_text_option).setVisible(!mSearchEnabled);
		menu.setGroupVisible(R.id.find_text_group, mSearchEnabled);

		boolean isShown =  !mSearchEnabled || (getResources().getConfiguration().orientation
				== Configuration.ORIENTATION_LANDSCAPE);
		getActionBar().setDisplayShowHomeEnabled(isShown);
		getActionBar().setDisplayShowTitleEnabled(isShown);

		if (mSearchEnabled) {
			View actionView = menu.findItem(R.id.find_text_action).getActionView();
			mSearchInputView = (EditText) actionView.findViewById(R.id.find_text_box);
			mFilterMatches = (TextView) actionView.findViewById(R.id.find_text_matches);

			if (mTempSearchInput != null) {
				mSearchInputView.setText(mTempSearchInput);
				mSearchInputView.setSelection(mSearchInputView.length());
				setTextSearchCaption();
			}
			
			if (mPrevSearchCriterion != null) {
				executeSearch(null, SearchAction.RESUME, null);
			}
			
			mSearchInputView.setOnEditorActionListener(new OnEditorActionListener() {
				@Override
				public boolean onEditorAction(TextView v, int actionId,
						KeyEvent event) {
					if (event == null || event.getAction() == KeyEvent.ACTION_UP) {
						String searchCriterion = mSearchInputView.getText().toString();
						if (!searchCriterion.equalsIgnoreCase(mPrevSearchCriterion)) {
							executeSearch(searchCriterion, SearchAction.BEGIN, null);
						} else {
							executeSearch(null, SearchAction.CONTINUE, Direction.NEXT);
						}
						mPrevSearchCriterion = searchCriterion;
						return true;
					}
					return false;
				}
			});
			mSearchInputView.setTypeface(TYPEFACE);
			mSearchInputView.requestFocus();
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
			mSearchEnabled = true;
			invalidateOptionsMenu();
			return true;
		case R.id.find_text_next:
			executeSearch(null, SearchAction.CONTINUE, Direction.NEXT);
			return true;
		case R.id.find_text_previous:
			executeSearch(null, SearchAction.CONTINUE, Direction.PREVIOUS);
			return true;
		case R.id.find_text_cancel:
			executeSearch(null, SearchAction.END, null);
			mConsoleInputView.requestFocus();
			return true;
		case R.id.save_history:
			if (mInputEnabled) {
				try {
					JSONArray consoleHistory = new JSONArray();
					for (ConsoleEntry entry : mConsoleEntries) {
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
				String title = getResources().getString(R.string.console_load_history_title);
				String message = getResources().getString(R.string.console_load_history_message);
				YesOrNoDialog exitDialog = new YesOrNoDialog(title, message) {
					@Override
					protected void yes(DialogInterface dialog, int whichButton) {
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
								mConsoleEntries.clear();
								for (int i = 0; i < consoleHistory.length(); i++) {
									JSONObject jsonEntry = consoleHistory.getJSONObject(i);
									int num = jsonEntry.getInt("num");
									String contents = jsonEntry.getString("contents");
									ConsoleEntry entry = new ConsoleEntry(num, contents);
									mConsoleEntries.add(entry);
								}
								mConsoleAdapter = new ConsoleEntryAdapter(ConsoleActivity.this, mConsoleEntries);
								mConsoleListView.setAdapter(mConsoleAdapter);
								updateConsoleEntries();

								JSONArray commandHistory = history.getJSONArray(COMMANDS_TAG);
								mCommandHistoryEntries.clear();
								for (int i = 0; i < commandHistory.length(); i++) {
									mCommandHistoryEntries.add(commandHistory.getString(i));
								}
								mCommandHistoryAdapter = new CommandHistoryAdapter(ConsoleActivity.this, mCommandHistoryEntries);
								mCommandHistoryListView.setAdapter(mCommandHistoryAdapter);
								updateCommandHistoryEntries();

								mConsoleInputView.requestFocus();
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
				};
				exitDialog.show(getFragmentManager(), "load");
			}
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
				menu.add(0, 42, 0, "Sample transformation (does nothing)");
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
					mConsoleInputView.setText(mTempCommand + " " + keywordNStr);
				}
			} else { //If long-click command is run
				if (mInputEnabled) {
					mDispatcher.runKeywordCommand(keywordNStr, keywordNStr);
				} else {
					mConsoleInputView.setText(CommandDispatcher.getKeyword(keywordNStr)
							.getCommand().getCommandName() + " " + keywordNStr);
				}
			}
			mConsoleInputView.requestFocus(); //Prevents ListView from stealing focus
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
		mConsoleEntries.add(entry);
		updateConsoleEntries();
		scrollToBottom();
	}

	void removeConsoleEntry() {
		if (!mConsoleEntries.isEmpty()) {
			mConsoleEntries.remove(getEntryCount() - 1);
			updateConsoleEntries();
			scrollToBottom();
		}
	}

	public void appendConsoleEntry(String newContents) {
		mConsoleEntries.get(getEntryCount() - 1).appendContents(newContents);
		updateConsoleEntries();
		scrollToBottom();
	}

	public void clear() {
		ConsoleClearer clear = new ConsoleClearer(this, mConsoleEntries);
		getEditManager().applyEdit(clear);
	}

	/**
	 * Removes all console entries and resets the entry count.
	 */
	void clearConsole() {
		mConsoleEntries.clear();
		updateConsoleEntries();
	}

	public void disableInput() {
		mInputEnabled = false;
	}

	public void enableInput() {
		mInputEnabled = true;
	}

	public int getEntryCount() {
		return mConsoleEntries.size();
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
		mConsoleInputView.setText(text);
		mConsoleInputView.setSelection(mConsoleInputView.getText().length());
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
		mConsoleEntries.get(mConsoleEntries.size() - 1).setWaiting(shown);
		updateConsoleEntries();
		scrollToBottom();
	}

	private void attemptWordCompletion() {
		String input = mConsoleInputView.getText().toString().trim();
		if (input.split(WHITESPACE).length <= 1) {
			String completion = mCompleter.completeWord(input);
			if (completion != null) {
				setInputText(completion);
			}
		}
	}
	
	private void executeSearch(String criterion, SearchAction action, Direction direction) {
		if (mInputEnabled) {
			setInputEnabled(false);
			MatchParams params = null;
			switch (action) {
			case BEGIN:
				params = mSearcher.beginSearch(criterion);
				break;
			case CONTINUE:
				params = mSearcher.continueSearch(direction);
			case RESUME:
				params = mSearcher.getSelectedMatch();
				break;
			case END:
				mSearchEnabled = false;
				mPrevSearchCriterion = null;
				mSearcher.endSearch();
				invalidateOptionsMenu();
				setInputEnabled(true);
				return;
			}
			if (params != null && !mConsoleListView.isEntryVisible(params.getListIndex())) {
				mConsoleListView.smoothScrollToPositionFromTop(params.getListIndex(),
						params.getTextViewOffset(), SCROLL_DURATION);
			}
			setTextSearchCaption();
			setInputEnabled(true);
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

	private void setTextSearchCaption() {
		int matches = mSearcher.getMatchesCount();
		String caption;
		if (matches > 0) {
			caption = mSearcher.getSelectedMatchPosition() + "/" + matches;
		} else {
			caption = "0";
		}
		mFilterMatches.setText(caption + " match" + (caption.endsWith("1") ? "" : "es"));
	}

	/**
	 * Refreshes the console entries, removing excessive entries from the top if ENTRY_LIMIT is exceeded.
	 */
	private void updateConsoleEntries() {
		if (mConsoleEntries.size() > ENTRY_CONSOLE_LIMIT) {
			mConsoleEntries.remove(0);
		}
		mConsoleAdapter.notifyDataSetChanged();
		updateEntryCount();
	}

	void updateConsoleEntries(List<ConsoleEntry> newEntries) {
		mConsoleEntries.clear();
		mConsoleEntries.addAll(newEntries);
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
	
	private enum SearchAction { BEGIN, CONTINUE, RESUME, END };

}