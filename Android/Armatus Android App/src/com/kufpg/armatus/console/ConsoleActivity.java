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

import com.kufpg.armatus.EditManager.Edit;
import com.kufpg.armatus.MainActivity;
import com.kufpg.armatus.R;
import com.kufpg.armatus.BaseActivity;
import com.kufpg.armatus.console.ConsoleEdits.Clear;
import com.kufpg.armatus.console.ConsoleEdits.AddEntry;
import com.kufpg.armatus.console.ConsoleSearcher.Direction;
import com.kufpg.armatus.console.ConsoleSearcher.MatchParams;
import com.kufpg.armatus.dialog.ConsoleEntrySelectionDialog;
import com.kufpg.armatus.dialog.GestureDialog;
import com.kufpg.armatus.dialog.KeywordSwapDialog;
import com.kufpg.armatus.dialog.WordCompletionDialog;
import com.kufpg.armatus.dialog.YesOrNoDialog;
import com.kufpg.armatus.drag.DragIcon;
import com.kufpg.armatus.util.JsonUtils;
import com.kufpg.armatus.util.StringUtils;
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
import android.view.View.MeasureSpec;
import android.view.View.OnClickListener;
import android.view.View.OnDragListener;
import android.view.View.OnLayoutChangeListener;
import android.view.View.OnLongClickListener;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.view.inputmethod.InputMethodManager;
import android.widget.AbsListView;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.EditText;
import android.widget.ExpandableListView;
import android.widget.ListView;
import android.widget.RelativeLayout;
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

	private RelativeLayout mConsoleLayout, mConsoleInputLayout;
	private ConsoleListView mConsoleListView;
	private ListView mCommandHistoryListView;
	private ExpandableListView mCommandExpandableMenuView;
	private ConsoleEntryAdapter mConsoleAdapter;
	private ConsoleSearcher mSearcher;
	private CommandHistoryAdapter mCommandHistoryAdapter;
	private CommandExpandableMenuAdapter mCommandExpandableMenuAdapter;
	private List<ConsoleEntry> mConsoleEntries;
	private List<String> mCommandHistoryEntries;
	private TextView mConsoleInputNumView, mFilterMatches;
	private ConsoleInputEditText mConsoleInputEditText;
	private View mConsoleEmptySpace;
	private EditText mSearchInputView;
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
	private int mConsoleInputNum = 0;
	private int mConsoleEntriesHeight, mConsoleInputHeight, mScreenHeight, mConsoleWidth;

	@SuppressWarnings("unchecked")
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		requestWindowFeature(Window.FEATURE_PROGRESS);
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console_sliding_menu_activity);

		mConsoleLayout = (RelativeLayout) findViewById(R.id.console_list_layout);
		mConsoleListView = (ConsoleListView) findViewById(R.id.console_list_view);
		mSlidingMenu = (SlidingMenu) findViewById(R.id.console_sliding_menu);
		mCommandHistoryListView = (ListView)findViewById(R.id.History);
		mCommandExpandableMenuView = (ExpandableListView) findViewById(R.id.command_expandable_menu);
		mConsoleEmptySpace = (View) findViewById(R.id.console_empty_space);
		final View rootView = ((ViewGroup) findViewById(android.R.id.content)).getChildAt(0);
		mConsoleInputLayout = (RelativeLayout) getLayoutInflater().inflate(R.layout.console_input, null);
		mConsoleInputNumView = (TextView) mConsoleInputLayout.findViewById(R.id.console_input_num);
		mConsoleInputEditText = (ConsoleInputEditText) mConsoleInputLayout.findViewById(R.id.console_input_edit_text);
		mCallback = new ConsoleEntryCallback(this);
		mDispatcher = new CommandDispatcher(this);
		TYPEFACE = Typeface.createFromAsset(getAssets(), TYPEFACE_PATH);

		if (savedInstanceState == null) {
			setSoftKeyboardVisible(true);
			mConsoleEntries = new ArrayList<ConsoleEntry>();
			mCommandHistoryEntries = new ArrayList<String>();
			mCompleter = new WordCompleter(this);
			mConsoleInputLayout.setLayoutParams(new AbsListView.LayoutParams(
					AbsListView.LayoutParams.MATCH_PARENT, AbsListView.LayoutParams.WRAP_CONTENT));
			mConsoleInputEditText.requestFocus();
		} else {
			mConsoleInputEditText.setText(savedInstanceState.getString("consoleInput"));
			mSoftKeyboardVisible = savedInstanceState.getBoolean("softKeyboardVisibility");
			setSoftKeyboardVisible(mSoftKeyboardVisible);
			mSearchEnabled = savedInstanceState.getBoolean("findTextEnabled");
			if (mSearchEnabled) {
				mTempSearchInput = savedInstanceState.getString("searchInput");
				mPrevSearchCriterion = savedInstanceState.getString("prevSearchCriterion");
			} else {
				mConsoleInputEditText.setSelection(savedInstanceState.getInt("consoleInputCursor"));
			}

			mConsoleInputNum = savedInstanceState.getInt("consoleInputNum");
			mConsoleEntries = (List<ConsoleEntry>) savedInstanceState.getSerializable("consoleEntries");
			mCommandHistoryEntries = (List<String>) savedInstanceState.getSerializable("commandEntries");
			mCompleter = (WordCompleter) savedInstanceState.getParcelable("wordCompleter");
			mCompleter.attachConsole(this);

			for (Edit edit : getEditManager()) {
				if (edit instanceof ConsoleEdit) {
					((ConsoleEdit) edit).attachConsole(this);
				}
			}
		}

		mConsoleLayout.addOnLayoutChangeListener(new OnLayoutChangeListener() {
			@Override
			public void onLayoutChange(View v, int left, int top, int right,
					int bottom, int oldLeft, int oldTop, int oldRight,
					int oldBottom) {
				mScreenHeight = mConsoleLayout.getMeasuredHeight();
				resizeEmptySpace();
			}
		});

		rootView.getViewTreeObserver().addOnGlobalLayoutListener(new OnGlobalLayoutListener() {
			@Override
			//Detects whether soft keyboard is open or closed
			public void onGlobalLayout() {
				int rootHeight = rootView.getRootView().getHeight();
				int heightDiff = rootHeight - rootView.getHeight();
				if (heightDiff > rootHeight/3) { //This works on Nexus 7s, at the very least
					mSoftKeyboardVisible = true;
				} else {
					mSoftKeyboardVisible = false;
				}
			}
		});

		mConsoleEmptySpace.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				if (!mSoftKeyboardVisible) {
					((InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE))
					.toggleSoftInput(InputMethodManager.SHOW_FORCED,0);
				}
			}
		});
		mConsoleEmptySpace.setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				mConsoleInputEditText.performLongClick();
				return false;
			}
		});
		mConsoleEmptySpace.setOnDragListener(new OnDragListener() {
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

		registerForContextMenu(mConsoleListView);
		mConsoleAdapter = new ConsoleEntryAdapter(this, mConsoleEntries);
		mConsoleListView.addFooterView(mConsoleInputLayout);
		mConsoleListView.setAdapter(mConsoleAdapter); //MUST be called after addFooterView()
		mConsoleListView.addOnLayoutChangeListener(new OnLayoutChangeListener() {
			@Override
			public void onLayoutChange(View v, int left, int top, int right,
					int bottom, int oldLeft, int oldTop, int oldRight,
					int oldBottom) {
				mConsoleEntriesHeight = 0;
				for (int i = 0; i < getEntryCount(); i++) {
					View child = mConsoleListView.getAdapter().getView(i, null, mConsoleListView);
					child.measure(MeasureSpec.makeMeasureSpec(mConsoleWidth, MeasureSpec.AT_MOST),
							MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));
					mConsoleEntriesHeight += child.getMeasuredHeight();
					if (mConsoleEntriesHeight >= mScreenHeight) {
						break;
					}
				}
				resizeEmptySpace();
			}
		});

		if (savedInstanceState == null) {
			mSearcher = new ConsoleSearcher(mConsoleAdapter);
		} else {
			mSearcher = savedInstanceState.getParcelable("consoleSearcher");
			mSearcher.attachAdapter(mConsoleAdapter);
		}

		mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistoryEntries);
		mCommandHistoryListView.setAdapter(mCommandHistoryAdapter);

		mCommandExpandableMenuAdapter = new CommandExpandableMenuAdapter(this);
		mCommandExpandableMenuView.setAdapter(mCommandExpandableMenuAdapter);

		mConsoleInputNumView.setTypeface(TYPEFACE);
		mConsoleInputEditText.setTypeface(TYPEFACE);
		mConsoleInputEditText.addTextChangedListener(mCompleter);
		mConsoleInputEditText.addTextChangedListener(new TextWatcher() {
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
				mConsoleListView.post(new Runnable() {
					public void run() {
						//DON'T use scrollToBottom(); it will cause a strange jaggedy scroll effect
						mConsoleListView.setSelection(mConsoleListView.getCount() - 1);
					}
				});
			}
		});
		//Processes user input (and runs command, if input is a command) when Enter is pressed
		mConsoleInputEditText.setOnEditorActionListener(new OnEditorActionListener() {
			@Override
			public boolean onEditorAction(TextView v, int actionId, KeyEvent event) {
				if (event == null || event.getAction() == KeyEvent.ACTION_UP) {
					if (mInputEnabled) {
						String[] inputs = mConsoleInputEditText.getText().toString().trim()
								.split(StringUtils.WHITESPACE);
						if (CommandDispatcher.isCommand(inputs[0])) {
							if (inputs.length == 1) {
								mDispatcher.runOnConsole(inputs[0]);
							} else {
								mDispatcher.runOnConsole(inputs[0], Arrays.copyOfRange(inputs, 1, inputs.length));
							}
						} else {
							addConsoleEntry(mConsoleInputEditText.getText().toString());
						}
						mConsoleInputEditText.setText("");
						return true;
					}

				}
				return false;
			}
		});
		mConsoleInputLayout.addOnLayoutChangeListener(new OnLayoutChangeListener() {
			@Override
			public void onLayoutChange(View v, int left, int top, int right,
					int bottom, int oldLeft, int oldTop, int oldRight,
					int oldBottom) {
				mConsoleInputHeight = mConsoleInputLayout.getMeasuredHeight();
				mConsoleWidth = mConsoleInputLayout.getMeasuredWidth();
				resizeEmptySpace();
			}
		});

		updateConsoleEntries();
		updateCommandHistoryEntries();
		refreshSlidingMenu();
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putInt("consoleInputNum", mConsoleInputNum);
		outState.putString("consoleInput", mConsoleInputEditText.getText().toString());
		outState.putParcelable("consoleSearcher", mSearcher);
		outState.putInt("consoleInputCursor", mConsoleInputEditText.getSelectionStart());
		if (mSearchInputView != null) {
			outState.putString("searchInput", mSearchInputView.getText().toString());
			outState.putString("prevSearchCriterion", mPrevSearchCriterion);
		}
		outState.putBoolean("softKeyboardVisibility", mSoftKeyboardVisible);
		outState.putBoolean("findTextEnabled", mSearchEnabled);
		outState.putSerializable("consoleEntries", (Serializable) mConsoleEntries);
		outState.putSerializable("commandEntries", (Serializable) mCommandHistoryEntries);
		outState.putParcelable("wordCompleter", mCompleter);
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
						String searchCriterion = StringUtils.applyCharWrap(mSearchInputView.getText().toString());
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
			mConsoleInputEditText.requestFocus();
			return true;
		case R.id.save_history:
			if (mInputEnabled) {
				try {
					JSONArray consoleHistory = new JSONArray();
					for (ConsoleEntry entry : mConsoleEntries) {
						JSONObject entryJson = new JSONObject();
						entryJson.put("num", entry.getNum());
						entryJson.put("contents", entry.getShortContents());
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

								mConsoleInputEditText.requestFocus();
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
				!mConsoleEntries.get(info.position).getShortContents().isEmpty()) { //To prevent empty lines
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
					mConsoleInputEditText.setText(mTempCommand + " " + keywordNStr);
				}
			} else { //If long-click command is run
				if (mInputEnabled) {
					mDispatcher.runKeywordCommand(keywordNStr, keywordNStr);
				} else {
					mConsoleInputEditText.setText(CommandDispatcher.getKeyword(keywordNStr)
							.getCommand().getCommandName() + " " + keywordNStr);
				}
			}
			mConsoleInputEditText.requestFocus(); //Prevents ListView from stealing focus
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
		AddEntry edit = new AddEntry(this, contents);
		getEditManager().applyEdit(edit);
	}

	void addConsoleEntry(ConsoleEntry entry) {
		mConsoleInputNum++;
		mConsoleEntries.add(entry);
		updateConsoleEntries();
		scrollToBottom();
	}

	void removeConsoleEntry() {
		if (!mConsoleEntries.isEmpty()) {
			mConsoleInputNum--;
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
		Clear clear = new Clear(this, mConsoleInputNum, mConsoleEntries);
		getEditManager().applyEdit(clear);
	}

	/**
	 * Removes all console entries and resets the entry count.
	 */
	void clearConsole() {
		mConsoleInputNum = 0;
		mConsoleEntries.clear();
		updateConsoleEntries();
	}

	public void disableInput() {
		mInputEnabled = false;
	}

	public void enableInput() {
		mInputEnabled = true;
	}

	/**
	 * @return the total number of console entries.
	 */
	public int getEntryCount() {
		return mConsoleEntries.size();
	}

	/**
	 * @return the entry number for the console input field.
	 */
	public int getInputNum() {
		return mConsoleInputNum;
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
		mConsoleInputEditText.setText(text);
		mConsoleInputEditText.setSelection(mConsoleInputEditText.getText().length());
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
			newFrag = WordCompletionDialog.newInstance(mCompleter.getWordSuggestions());
		}
		ft.add(newFrag, tag);
		ft.commit();
	}

	private void attemptWordCompletion() {
		String input = mConsoleInputEditText.getText().toString().trim();
		if (input.split(StringUtils.WHITESPACE).length <= 1) {
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
			if (params != null && !mConsoleListView.isEntryVisible(params.listIndex)) {
				mConsoleListView.smoothScrollToPositionFromTop(params.listIndex,
						params.textViewOffset, SCROLL_DURATION);
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

	synchronized void resizeEmptySpace() {
		mConsoleEmptySpace.post(new Runnable() {
			@Override
			public void run() {
				mConsoleEmptySpace.getLayoutParams().height = mScreenHeight - mConsoleEntriesHeight - mConsoleInputHeight;
				mConsoleEmptySpace.requestLayout();
			}
		});
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
		updateInput();

		resizeEmptySpace();
	}

	void updateConsoleEntries(int inputNum, List<ConsoleEntry> newEntries) {
		mConsoleInputNum = inputNum;
		mConsoleEntries.clear();
		mConsoleEntries.addAll(newEntries);
		updateConsoleEntries();
	}

	private void updateCommandHistoryEntries() {
		if (mCommandHistoryEntries.size() > ENTRY_COMMAND_HISTORY_LIMIT) {
			mCommandHistoryEntries.remove(0);
		}
		mCommandHistoryAdapter.notifyDataSetChanged();
	}

	private void updateInput() {
		mConsoleInputNumView.setText("hermit<" + mConsoleInputNum + "> ");
		mConsoleInputNumView.measure(MeasureSpec.UNSPECIFIED, MeasureSpec.UNSPECIFIED);
		final int width = mConsoleInputNumView.getMeasuredWidth();
		final int padding = mConsoleInputNumView.getPaddingLeft();
		mConsoleInputEditText.setIndent(width - padding);
	}

	private enum SearchAction { BEGIN, CONTINUE, RESUME, END };

}