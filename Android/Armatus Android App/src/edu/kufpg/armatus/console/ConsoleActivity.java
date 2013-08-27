package edu.kufpg.armatus.console;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import com.google.common.collect.ListMultimap;
import com.jeremyfeinstein.slidingmenu.lib.SlidingMenu;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.MainActivity;
import edu.kufpg.armatus.PrefsActivity;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.command.CustomCommandDispatcher;
import edu.kufpg.armatus.console.ConsoleEntryAdapter.ConsoleEntryHolder;
import edu.kufpg.armatus.console.ConsoleSearcher.MatchParams;
import edu.kufpg.armatus.console.ConsoleSearcher.SearchDirection;
import edu.kufpg.armatus.dialog.ConsoleEntrySelectionDialog;
import edu.kufpg.armatus.dialog.GestureDialog;
import edu.kufpg.armatus.dialog.KeywordSwapDialog;
import edu.kufpg.armatus.dialog.WordCompletionDialog;
import edu.kufpg.armatus.dialog.YesOrNoDialog;
import edu.kufpg.armatus.networking.BluetoothDeviceListActivity;
import edu.kufpg.armatus.networking.BluetoothUtils;
import edu.kufpg.armatus.networking.InternetUtils;
import edu.kufpg.armatus.util.JsonUtils;
import edu.kufpg.armatus.util.StringUtils;
import android.app.DialogFragment;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.graphics.Typeface;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.ContextMenu;
import android.view.DragEvent;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.MeasureSpec;
import android.view.View.OnClickListener;
import android.view.View.OnDragListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.view.Window;
import android.view.WindowManager;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.widget.AbsListView;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.EditText;
import android.widget.ExpandableListView;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.TextView.OnEditorActionListener;

/**
 * Activity that displays an interactive, feature-rich
 * (at least it will be some day) HERMIT console.
 */
public class ConsoleActivity extends BaseActivity {

	public static final int DEFAULT_FONT_SIZE = 15;
	public static final int CONSOLE_ENTRY_LIMIT = 100;
	public static final int COMMAND_HISTORY_ENTRY_LIMIT = 200;
	private static final int LONG_CLICKED_GROUP = 42;
	private static final int DRAGGED_GROUP = 43;
	private static final int SCROLL_DURATION = 500;
	private static final String SELECTION_TAG = "selection";
	private static final String KEYWORD_SWAP_TAG = "keywordswap";
	private static final String WORD_COMPLETION_TAG = "wordcomplete";
	private static final String CONSOLE_HISTORY_TAG = "console";
	private static final String COMMANDS_HISTORY_TAG = "commands";
	private static final String SESSION_HISTORY_FILENAME = "/history.txt";
	public static Typeface TYPEFACE;
	private static final String TYPEFACE_PATH = "fonts/DroidSansMonoDotted.ttf";

	private HermitClient mHermitClient;
	private RelativeLayout mConsoleInputLayout;
	private ConsoleListView mConsoleListView;
	private ListView mCommandHistoryListView;
	private ExpandableListView mCommandExpandableMenuView;
	private ConsoleEntryAdapter mConsoleListAdapter;
	private ConsoleSearcher mSearcher;
	private CommandHistoryAdapter mCommandHistoryAdapter;
	private CommandExpandableMenuAdapter mCommandExpandableMenuAdapter;
	private List<ConsoleEntry> mConsoleEntries;
	private List<String> mCommandHistoryEntries;
	private TextView mConsoleInputNumView, mSearchMatches;
	private ConsoleInputEditText mConsoleInputEditText;
	private View mConsoleEmptySpace;
	private EditText mSearchInputView;
	private SlidingMenu mSlidingMenu;
	private WordCompleter mCompleter;
	private String mTempCommand, mTempSearchInput, mPrevSearchCriterion;
	private JSONObject mHistory;
	private boolean mInputEnabled = true;
	private boolean mSoftKeyboardVisibility = true;
	private boolean mSearchEnabled = false;
	private int mConsoleInputNum = 0;
	private SharedPreferences mPrefs;

	@SuppressWarnings("unchecked")
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		requestWindowFeature(Window.FEATURE_PROGRESS);
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console_sliding_menu_activity);
		setProgressBarIndeterminate(true);

		mConsoleListView = (ConsoleListView) findViewById(R.id.console_list_view);
		mSlidingMenu = (SlidingMenu) findViewById(R.id.console_sliding_menu);
		mCommandHistoryListView = (ListView) findViewById(R.id.command_history_list);
		mCommandExpandableMenuView = (ExpandableListView) findViewById(R.id.command_expandable_menu);
		mConsoleEmptySpace = (View) findViewById(R.id.console_empty_space);
		mSearchMatches = (TextView) findViewById(R.id.console_search_matches_indicator);
		final View rootView = ((ViewGroup) findViewById(android.R.id.content)).getChildAt(0);
		mConsoleInputLayout = (RelativeLayout) getLayoutInflater().inflate(R.layout.console_input, null);
		mConsoleInputNumView = (TextView) mConsoleInputLayout.findViewById(R.id.console_input_num);
		mConsoleInputEditText = (ConsoleInputEditText) mConsoleInputLayout.findViewById(R.id.console_input_edit_text);
		mPrefs = PrefsActivity.getPrefs(this);
		TYPEFACE = Typeface.createFromAsset(getAssets(), TYPEFACE_PATH);

		if (savedInstanceState == null) {
			setSoftKeyboardVisibility(true);
			mConsoleEntries = new ArrayList<ConsoleEntry>();
			mCommandHistoryEntries = new ArrayList<String>();
			mConsoleInputLayout.setLayoutParams(new AbsListView.LayoutParams(
					AbsListView.LayoutParams.MATCH_PARENT, AbsListView.LayoutParams.WRAP_CONTENT));
			mConsoleInputEditText.requestFocus();
		} else {
			mConsoleInputEditText.setText(savedInstanceState.getString("consoleInput"));
			mSoftKeyboardVisibility = savedInstanceState.getBoolean("softKeyboardVisibility");
			setSoftKeyboardVisibility(mSoftKeyboardVisibility);
			mSearchEnabled = savedInstanceState.getBoolean("findTextEnabled");
			if (mSearchEnabled) {
				mSearchMatches.setVisibility(View.VISIBLE);
				mSearchMatches.setText(savedInstanceState.getString("searchMatches"));
				mTempSearchInput = savedInstanceState.getString("searchInput");
				mPrevSearchCriterion = savedInstanceState.getString("prevSearchCriterion");
			} else {
				mConsoleInputEditText.setSelection(savedInstanceState.getInt("consoleInputCursor"));
			}

			mConsoleInputNum = savedInstanceState.getInt("consoleInputNum");
			mConsoleEntries = (List<ConsoleEntry>) savedInstanceState.getSerializable("consoleEntries");
			mCommandHistoryEntries = (List<String>) savedInstanceState.getSerializable("commandEntries");
			mCommandExpandableMenuAdapter = new CommandExpandableMenuAdapter(this, null, null);
			mCommandExpandableMenuView.setAdapter(mCommandExpandableMenuAdapter);
			mCompleter = (WordCompleter) savedInstanceState.getParcelable("wordCompleter");
			mCompleter.attachConsole(this);
		}

		rootView.getViewTreeObserver().addOnGlobalLayoutListener(new OnGlobalLayoutListener() {
			@Override
			//Detects whether soft keyboard is open or closed
			public void onGlobalLayout() {
				int rootHeight = rootView.getRootView().getHeight();
				int heightDiff = rootHeight - rootView.getHeight();
				if (heightDiff > rootHeight/3) { //This works on Nexus 7s, at the very least
					mSoftKeyboardVisibility = true;
				} else {
					mSoftKeyboardVisibility = false;
				}
			}
		});

		mConsoleEmptySpace.setOnTouchListener(new OnTouchListener() {
			@Override
			public boolean onTouch(View v, MotionEvent event) {
				/* Allows empty space to serve as "continuation" of the input EditText, so
				 * if the user clicks or long-clicks the empty space, then the input
				 * EditText will receive the touch event.
				 */
				return mConsoleInputEditText.dispatchTouchEvent(event);
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
					if (event.getLocalState() instanceof View) {
						View dragSource = (View) event.getLocalState();
						if (dragSource != null) {
							dragSource.setVisibility(View.VISIBLE);
						}
					}
					return true;
				default:
					return false;
				}
			}
		});

		registerForContextMenu(mConsoleListView);
		mConsoleListAdapter = new ConsoleEntryAdapter(this, mConsoleEntries);
		mConsoleListView.addFooterView(mConsoleInputLayout);
		mConsoleListView.setAdapter(mConsoleListAdapter); //MUST be called after addFooterView()

		if (savedInstanceState == null) {
			mSearcher = new ConsoleSearcher(mConsoleListAdapter);
		} else {
			mSearcher = savedInstanceState.getParcelable("consoleSearcher");
			mSearcher.attachAdapter(mConsoleListAdapter);
		}

		mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistoryEntries);
		mCommandHistoryListView.setAdapter(mCommandHistoryAdapter);

		mConsoleInputNumView.setTypeface(TYPEFACE);
		mConsoleInputEditText.setTypeface(TYPEFACE);
		mConsoleInputEditText.addTextChangedListener(new TextWatcher() {
			@Override
			public void afterTextChanged(Editable s) {}
			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {}
			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {
				//Closes SlidingMenu and scroll to bottom if user begins typing
				mSlidingMenu.showContent();
				mConsoleListView.setActionModeVisible(false);
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
						String input = mConsoleInputEditText.getText().toString();
						if (input.isEmpty() || input.matches(StringUtils.WHITESPACE)) {
							addConsoleEntry(input);
						} else {
							String[] inputs = input.trim().split(StringUtils.WHITESPACE);
							if (inputs.length == 1) {
								CustomCommandDispatcher.runCustomCommand(ConsoleActivity.this, inputs[0]);
							} else {
								CustomCommandDispatcher.runCustomCommand(ConsoleActivity.this, inputs[0],
										Arrays.copyOfRange(inputs, 1, inputs.length));
							}
						}
						mConsoleInputEditText.setText("");
						return true;
					}

				}
				return false;
			}
		});

		updateConsoleEntries();
		updateCommandHistoryEntries();
		resizeSlidingMenu();

		mHermitClient = new HermitClient(this);
		if (savedInstanceState == null) {
			mHermitClient.connect();
		}
	}

	void initCommandRelatedVariables(SortedSet<String> commandDictionary,
			List<String> tagList, ListMultimap<String, String> tagMap) {
		mCompleter = new WordCompleter(this, commandDictionary);
		mConsoleInputEditText.addTextChangedListener(mCompleter);
		mCommandExpandableMenuAdapter = new CommandExpandableMenuAdapter(this, tagList, tagMap);
		mCommandExpandableMenuView.setAdapter(mCommandExpandableMenuAdapter);
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putInt("consoleInputNum", mConsoleInputNum);
		outState.putString("consoleInput", mConsoleInputEditText.getText().toString());
		outState.putParcelable("consoleSearcher", mSearcher);
		outState.putInt("consoleInputCursor", mConsoleInputEditText.getSelectionStart());
		if (mSearchInputView != null) {
			outState.putString("searchMatches", mSearchMatches.getText().toString());
			outState.putString("searchInput", mSearchInputView.getText().toString());
			outState.putString("prevSearchCriterion", mPrevSearchCriterion);
		}
		outState.putBoolean("softKeyboardVisibility", mSoftKeyboardVisibility);
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
				exit();
			}
		};
		exitDialog.show(getFragmentManager(), "exit");
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		switch (requestCode) {
		case BluetoothUtils.REQUEST_ENABLE_BLUETOOTH:
			if (mHermitClient.isRequestDelayed()) {
				if (resultCode == RESULT_OK) {
					mHermitClient.runDelayedRequest();
				} else {
					appendConsoleEntry("ERROR: Failed to enable Bluetooth.");
				}
			}
			break;
		case InternetUtils.REQUEST_ENABLE_WIFI:
			if (mHermitClient.isRequestDelayed()) {
				//Unfortunately, ACTION_PICK_WIFI_NETWORK doesn't use RESULT_OK, so we
				//have to check the Wi-Fi state manually.
				if (InternetUtils.isWifiConnected(this)) {
					mHermitClient.runDelayedRequest();
				} else {
					appendConsoleEntry("ERROR: Failed to enable Wi-Fi.");
				}
			}
			break;
		case BluetoothUtils.REQUEST_FIND_BLUETOOTH_DEVICE:
			if (mHermitClient.isRequestDelayed()) {
				if (resultCode == RESULT_OK) {
					String name = data.getStringExtra(BluetoothDeviceListActivity.EXTRA_DEVICE_NAME);
					String address = data.getStringExtra(BluetoothDeviceListActivity.EXTRA_DEVICE_ADDRESS);
					BluetoothUtils.setBluetoothDeviceInfo(this, name, address);
					mHermitClient.runDelayedRequest();
				} else {
					appendConsoleEntry("ERROR: Failed to locate Bluetooth device.");
				}
			}
			break;
		}
		super.onActivityResult(requestCode, resultCode, data);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);

		menu.setGroupVisible(R.id.menu_icons_group, !mSearchEnabled);
		menu.setGroupVisible(R.id.history_group, true);
		menu.findItem(R.id.find_text_option).setVisible(!mSearchEnabled);
		menu.findItem(R.id.find_text_action).setVisible(mSearchEnabled);

		if (mSearchEnabled) {
			View actionView = menu.findItem(R.id.find_text_action).getActionView();
			mSearchInputView = (EditText) actionView.findViewById(R.id.find_text_box);
			final ImageButton nextButton, prevButton, cancelButton;
			nextButton = (ImageButton) actionView.findViewById(R.id.find_text_next);
			prevButton = (ImageButton) actionView.findViewById(R.id.find_text_previous);
			cancelButton = (ImageButton) actionView.findViewById(R.id.find_text_cancel);
			final OnClickListener actionLayoutButtonListener = new OnClickListener() {
				@Override
				public void onClick(View v) {
					switch (v.getId()) {
					case R.id.find_text_next:
						executeSearch(null, SearchAction.CONTINUE, SearchDirection.NEXT);
						break;
					case R.id.find_text_previous:
						executeSearch(null, SearchAction.CONTINUE, SearchDirection.PREVIOUS);
						break;
					case R.id.find_text_cancel:
						executeSearch(null, SearchAction.END, null);
						mConsoleInputEditText.requestFocus();
						break;
					}
				}
			};
			nextButton.setOnClickListener(actionLayoutButtonListener);
			prevButton.setOnClickListener(actionLayoutButtonListener);
			cancelButton.setOnClickListener(actionLayoutButtonListener);

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
						String searchCriterion = StringUtils.withCharWrap(mSearchInputView.getText().toString());
						if (!searchCriterion.equalsIgnoreCase(mPrevSearchCriterion)) {
							executeSearch(searchCriterion, SearchAction.BEGIN, null);
						} else {
							executeSearch(null, SearchAction.CONTINUE, SearchDirection.NEXT);
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
					mHistory.put(CONSOLE_HISTORY_TAG, consoleHistory);
					mHistory.put(COMMANDS_HISTORY_TAG, commandHistory);

					String path = "";
					if (mPrefs.getBoolean(IS_HISTORY_DIR_CUSTOM_KEY, true)) {
						path = mPrefs.getString(HISTORY_DIR_KEY, null);
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
						if (mPrefs.getBoolean(IS_HISTORY_DIR_CUSTOM_KEY, true)) {
							path = mPrefs.getString(HISTORY_DIR_KEY, null);
						} else {
							path = CACHE_DIR;
						}

						final File file = new File (path + SESSION_HISTORY_FILENAME);
						if (file.exists()) {
							JSONObject history = null;
							try {
								history = JsonUtils.openJsonFile(file.getAbsolutePath());

								JSONArray consoleHistory = history.getJSONArray(CONSOLE_HISTORY_TAG);
								mConsoleEntries.clear();
								for (int i = 0; i < consoleHistory.length(); i++) {
									JSONObject jsonEntry = consoleHistory.getJSONObject(i);
									int num = jsonEntry.getInt("num");
									String contents = jsonEntry.getString("contents");
									ConsoleEntry entry = new ConsoleEntry(num, contents);
									mConsoleEntries.add(entry);
								}
								mConsoleListAdapter = new ConsoleEntryAdapter(ConsoleActivity.this, mConsoleEntries);
								mConsoleListView.setAdapter(mConsoleListAdapter);
								updateConsoleEntries();

								JSONArray commandHistory = history.getJSONArray(COMMANDS_HISTORY_TAG);
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
				!mConsoleEntries.get(info.position).getShortContents().toString().isEmpty()) { //To prevent empty lines
			super.onCreateContextMenu(menu, v, menuInfo);

			final int group;
			mTempCommand = ((ConsoleEntryHolder) info.targetView.getTag()).draggedOverCommand;
			if (mTempCommand != null) { //If user dragged CommandIcon onto entry
				menu.setHeaderTitle("Entry " + info.position + ": Execute " + mTempCommand + " on...");
				group = DRAGGED_GROUP;
			} else { //If user long-clicked entry
				menu.setHeaderTitle("Entry " + info.position + ": Commands found");
				menu.add(Menu.NONE, Menu.NONE, 1, "Sample transformation (does nothing)");
				group = LONG_CLICKED_GROUP;
			}

			int order = 2;
			for (String keyword : mConsoleEntries.get(info.position).getKeywords()) {
				menu.add(group, v.getId(), order, keyword);
				order++;
			}
		}
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		if (item != null) {
			String keywordNStr = item.getTitle().toString();
			switch (item.getGroupId()) {
			case DRAGGED_GROUP:
				if (mInputEnabled) {
					CustomCommandDispatcher.runCustomCommand(this, mTempCommand, keywordNStr);
				} else {
					mConsoleInputEditText.setText(mTempCommand + StringUtils.NBSP + keywordNStr);
				}
				break;
			case LONG_CLICKED_GROUP:
				break;
			}
			mConsoleInputEditText.requestFocus(); //Prevents ListView from stealing focus
		}
		return super.onContextItemSelected(item);
	}

	@Override
	public void onContextMenuClosed(Menu menu) {
		//Ensures that this temp variable does not persist to next context menu opening
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
				|| commandName != mCommandHistoryEntries.get(0))) {
			mCommandHistoryEntries.add(0, commandName);
			updateCommandHistoryEntries();
		}
	}

	public void addConsoleEntry(String contents) {
		ConsoleEntry entry = new ConsoleEntry(getInputNum(), contents);
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

	public void appendConsoleEntry(CharSequence newContents) {
		mConsoleEntries.get(getEntryCount() - 1).appendContents(newContents);
		updateConsoleEntries();
		scrollToBottom();
	}

	public void clear() {
		mConsoleInputNum = 0;
		mConsoleEntries.clear();
		updateConsoleEntries();
	}

	public void disableInput() {
		mInputEnabled = false;
		mConsoleInputLayout.setVisibility(View.INVISIBLE);
	}

	public void enableInput() {
		mInputEnabled = true;
		mConsoleInputLayout.setVisibility(View.VISIBLE);
		mConsoleInputEditText.requestFocus();
	}

	public void exit() {
		if (mPrefs.getString(NETWORK_SOURCE_KEY, null).equals(NETWORK_SOURCE_BLUETOOTH_SERVER)) {
			if (BluetoothUtils.isBluetoothConnected(this)) {
				BluetoothUtils.closeBluetooth();
			}
		}
		Intent intent = new Intent(this, MainActivity.class);
		intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
		startActivity(intent);
	}

	public List<ConsoleEntry> getEntries() {
		return mConsoleEntries;
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

	public boolean isSoftKeyboardVisible() {
		return mSoftKeyboardVisibility;
	}

	public void setInputEnabled(boolean enabled) {
		mInputEnabled = enabled;
	}

	public void setInputText(String text) {
		mConsoleInputEditText.setText(text);
		mConsoleInputEditText.setSelection(mConsoleInputEditText.getText().length());
	}

	public void showEntrySelectionDialog(int... entries) {
		showDialog(ConsoleEntrySelectionDialog.newInstance(entries), SELECTION_TAG);
	}

	public void showKeywordSwapDialog(int entryNum, String entryContents) {
		showDialog(KeywordSwapDialog.newInstance(entryNum, entryContents), KEYWORD_SWAP_TAG);
	}

	public void showWordCompletionDialog(List<String> suggestions) {
		showDialog(WordCompletionDialog.newInstance(mCompleter.getWordSuggestions()), WORD_COMPLETION_TAG);
	}

	private void showDialog(DialogFragment fragment, String tag) {
		FragmentTransaction ft = getFragmentManager().beginTransaction();
		Fragment prev = getFragmentManager().findFragmentByTag(tag);
		if (prev != null) {
			ft.remove(prev);
		}
		ft.addToBackStack(null);
		ft.add(fragment, tag);
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

	private void executeSearch(String criterion, SearchAction action, SearchDirection direction) {
		if (mInputEnabled) {
			setInputEnabled(false);
			MatchParams params = null;
			switch (action) {
			case BEGIN:
				mSearchMatches.setVisibility(View.VISIBLE);
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
				mSearchMatches.setVisibility(View.GONE);
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

	void resizeEmptySpace(int newHeight) {
		mConsoleEmptySpace.getLayoutParams().height = newHeight;
		mConsoleEmptySpace.requestLayout();
	}

	/**
	 * Changes the SlidingMenu width depending on the screen size.
	 */
	private void resizeSlidingMenu() {
		Drawable command = getResources().getDrawable(R.drawable.template_white);
		int width = command.getIntrinsicWidth();
		mSlidingMenu.setBehindWidth(Math.round(width + 0.5f*width));
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
	public void setSoftKeyboardVisibility(boolean visible) {
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
		mSearchMatches.setText(caption + " match" + (caption.endsWith("1") ? "" : "es"));
	}

	/**
	 * Refreshes the console entries, removing excessive entries from the top if ENTRY_LIMIT is exceeded.
	 */
	private void updateConsoleEntries() {
		if (mConsoleEntries.size() > CONSOLE_ENTRY_LIMIT) {
			mConsoleEntries.remove(0);
		}
		mConsoleListAdapter.notifyDataSetChanged();
		updateInput();
	}

	void updateConsoleEntries(int inputNum, List<ConsoleEntry> newEntries) {
		mConsoleInputNum = inputNum;
		mConsoleEntries.clear();
		mConsoleEntries.addAll(newEntries);
		updateConsoleEntries();
	}

	private void updateCommandHistoryEntries() {
		if (mCommandHistoryEntries.size() > COMMAND_HISTORY_ENTRY_LIMIT) {
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