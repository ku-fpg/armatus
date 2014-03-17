package edu.kufpg.armatus.console;

import android.app.DialogFragment;
import android.app.Fragment;
import android.app.FragmentTransaction;
import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
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
import android.view.ViewGroup.LayoutParams;
import android.view.ViewTreeObserver.OnGlobalLayoutListener;
import android.view.Window;
import android.view.WindowManager;
import android.view.inputmethod.InputMethodManager;
import android.widget.AbsListView;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.EditText;
import android.widget.ExpandableListView;
import android.widget.ExpandableListView.ExpandableListContextMenuInfo;
import android.widget.ExpandableListView.OnChildClickListener;
import android.widget.ImageButton;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.TextView.OnEditorActionListener;
import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.common.collect.Lists;
import com.jeremyfeinstein.slidingmenu.lib.SlidingMenu;
import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.BaseApplication;
import edu.kufpg.armatus.MainActivity;
import edu.kufpg.armatus.Prefs;
import edu.kufpg.armatus.Prefs.NetworkSource;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.CommandExpandableMenuAdapter.CommandExpandableMenuItem;
import edu.kufpg.armatus.console.ConsoleWordSearcher.MatchParams;
import edu.kufpg.armatus.console.ConsoleWordSearcher.SearchDirection;
import edu.kufpg.armatus.data.CommandInfo;
import edu.kufpg.armatus.data.CommandResponse;
import edu.kufpg.armatus.data.HistoryCommand;
import edu.kufpg.armatus.dialog.CommandHelpDialog;
import edu.kufpg.armatus.dialog.ConsoleEntrySelectionDialog;
import edu.kufpg.armatus.dialog.GestureDialog;
import edu.kufpg.armatus.dialog.InputCompletionDialog;
import edu.kufpg.armatus.dialog.KeywordSwapDialog;
import edu.kufpg.armatus.dialog.ScrollEntriesDialog;
import edu.kufpg.armatus.dialog.YesOrNoDialog;
import edu.kufpg.armatus.input.SpecialKeyAdapter;
import edu.kufpg.armatus.networking.BluetoothDeviceListActivity;
import edu.kufpg.armatus.networking.BluetoothUtils;
import edu.kufpg.armatus.networking.InternetUtils;
import edu.kufpg.armatus.util.OnExpandableItemLongClickListener;
import edu.kufpg.armatus.util.StringUtils;
import edu.kufpg.armatus.util.Views;
import org.lucasr.twowayview.TwoWayView;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public class ConsoleActivity extends BaseActivity {

	public static final int DEFAULT_FONT_SIZE = 15;
	public static final int CONSOLE_ENTRY_LIMIT = 100;
	private static final int LONG_CLICKED_GROUP = 42;
	private static final int DRAGGED_GROUP = 43;
	private static final int SCROLL_DURATION = 500;
	private static final String SCROLL_ENTRIES_TAG = "scrollentries";
	private static final String SELECTION_TAG = "selection";
	private static final String KEYWORD_SWAP_TAG = "keywordswap";
	private static final String WORD_COMPLETION_TAG = "wordcomplete";
	public static Typeface TYPEFACE;
	private static final String TYPEFACE_PATH = "fonts/DroidSansMonoDotted.ttf";

	private HermitClient mHermitClient;
	protected RelativeLayout mConsoleListLayout;
	private RelativeLayout mConsoleInputLayout, mConsoleOptionsBar;
	protected ConsoleListView mConsoleListView;
	private ConsoleEntryAdapter mConsoleListAdapter;
	private ArrayList<ConsoleEntry> mConsoleEntries;
	private ListView mCommandHistoryView;
	private CommandHistoryAdapter mCommandHistoryAdapter;
	private ArrayList<HistoryCommand> mCommandHistory;
	protected ExpandableListView mCommandExpandableMenuView;
	private CommandExpandableMenuAdapter mCommandExpandableMenuAdapter;
	protected EditText mCommandExpandableSearchView;
	private ConsoleWordSearcher mSearcher;
	private CharSequence mMenuSearchConstraint;
	private ConsoleEntry mNextConsoleEntry;
	private ArrayList<String> mUserInputHistory;
	private int mUserInputHistoryChoice;
	private TextView mConsoleInputNumView, mSearchMatches;
	private ConsoleInputEditText mConsoleInputEditText;
	private View mConsoleEmptySpace;
	private EditText mSearchInputView;
	protected SlidingMenu mSlidingMenu;
	private String mTempCommand, mTempSearchInput, mPrevSearchCriterion;
	private boolean mInputEnabled = true;
	private boolean mSoftKeyboardVisible = true;
	private boolean mSearchEnabled = false;
	private int mConsoleInputNum = 0;
	private int mConsoleEntriesHeight, mConsoleInputHeight, mScreenHeight, mConsoleWidth;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		requestWindowFeature(Window.FEATURE_PROGRESS);
		super.onCreate(savedInstanceState);
		getActionBar().setDisplayHomeAsUpEnabled(true);
		setContentView(R.layout.console_sliding_menu_activity);
		setProgressBarIndeterminate(true);

		mConsoleListLayout = (RelativeLayout) findViewById(R.id.console_list_layout);
		mConsoleListView = (ConsoleListView) findViewById(R.id.console_list_view);
		mSlidingMenu = (SlidingMenu) findViewById(R.id.console_sliding_menu);
		mCommandHistoryView = (ListView) findViewById(R.id.command_history_list_view);
		mCommandExpandableMenuView = (ExpandableListView) findViewById(R.id.command_expandable_menu);
		mCommandExpandableSearchView = (EditText) findViewById(R.id.command_expandable_menu_search);
		mConsoleEmptySpace = findViewById(R.id.console_empty_space);
		mSearchMatches = (TextView) findViewById(R.id.console_search_matches_indicator);
		final View rootView = ((ViewGroup) findViewById(android.R.id.content)).getChildAt(0);
		mConsoleInputLayout = (RelativeLayout) getLayoutInflater().inflate(R.layout.console_input, null);
		mConsoleInputNumView = (TextView) mConsoleInputLayout.findViewById(R.id.console_input_num);
		mConsoleInputEditText = (ConsoleInputEditText) mConsoleInputLayout.findViewById(R.id.console_input_edit_text);
		mConsoleOptionsBar = (RelativeLayout) findViewById(R.id.console_options_bar);
		final ImageButton nextEntryButton = (ImageButton) findViewById(R.id.console_input_next_entry);
		final ImageButton scrollEntriesButton = (ImageButton) findViewById(R.id.console_input_scroll_entries);
		final ImageButton prevEntryButton = (ImageButton) findViewById(R.id.console_input_previous_entry);
		final ImageButton toggleSpecialKeysButton = (ImageButton) findViewById(R.id.console_options_toggle_special_keys);
		final ImageButton hideOptionsBarButton = (ImageButton) findViewById(R.id.console_options_hide_button);
		final TwoWayView specialKeyRow = (TwoWayView) findViewById(R.id.console_special_key_list);
		TYPEFACE = Typeface.createFromAsset(getAssets(), TYPEFACE_PATH);

		if (savedInstanceState == null) {
			setSoftKeyboardVisibility(true);
			mCommandHistory = new ArrayList<HistoryCommand>();
			mConsoleEntries = new ArrayList<ConsoleEntry>();
			mConsoleInputLayout.setLayoutParams(new AbsListView.LayoutParams(
					LayoutParams.MATCH_PARENT, LayoutParams.WRAP_CONTENT));
			mConsoleInputEditText.requestFocus();
			mHermitClient = new HermitClient(this);
			mUserInputHistory = new ArrayList<String>();
			mUserInputHistory.add("");
			mUserInputHistoryChoice = 0;
		} else {
			mConsoleInputEditText.setText(savedInstanceState.getString("consoleInput"));
			mSoftKeyboardVisible = savedInstanceState.getBoolean("softKeyboardVisibility");
			setSoftKeyboardVisibility(mSoftKeyboardVisible);
			mSearchEnabled = savedInstanceState.getBoolean("findTextEnabled");
			if (mSearchEnabled) {
				mSearchMatches.setVisibility(View.VISIBLE);
				mSearchMatches.setText(savedInstanceState.getString("searchMatches"));
				mTempSearchInput = savedInstanceState.getString("searchInput");
				mPrevSearchCriterion = savedInstanceState.getString("prevSearchCriterion");
			} else {
				mConsoleInputEditText.setSelection(savedInstanceState.getInt("consoleInputCursor"));
			}

			mCommandHistory = savedInstanceState.getParcelableArrayList("commandHistory");
			mConsoleInputNum = savedInstanceState.getInt("consoleInputNum");
			mConsoleEntries = savedInstanceState.getParcelableArrayList("consoleEntries");
			mHermitClient = savedInstanceState.getParcelable("hermitClient");
			mHermitClient.attachConsole(this);
			mUserInputHistory = savedInstanceState.getStringArrayList("userInputHistory");
			mUserInputHistoryChoice = savedInstanceState.getInt("userInputHistoryChoice");
		}

		mConsoleListLayout.addOnLayoutChangeListener(new OnLayoutChangeListener() {
			@Override
			public void onLayoutChange(View v, int left, int top, int right,
					int bottom, int oldLeft, int oldTop, int oldRight,
					int oldBottom) {
				mScreenHeight = mConsoleListLayout.getMeasuredHeight();
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

				specialKeyRow.setVisibility((mSoftKeyboardVisible && Prefs.getSpecialKeysVisible(ConsoleActivity.this))
						? View.VISIBLE : View.GONE);
			}
		});

		registerForContextMenu(mConsoleEmptySpace);
		mConsoleEmptySpace.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mConsoleInputEditText.setSelection(getInputLength());
				mConsoleInputEditText.requestFocus();
				if (!mSoftKeyboardVisible) {
					//setSoftKeyboardVisibility(true) doesn't work here for some weird reason
					((InputMethodManager)getSystemService(Context.INPUT_METHOD_SERVICE)).
					toggleSoftInput(InputMethodManager.SHOW_FORCED, InputMethodManager.HIDE_IMPLICIT_ONLY);
				}
			}
		});
		mConsoleEmptySpace.setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				openContextMenu(mConsoleEmptySpace);
				return true;
			}
		});
		mConsoleEmptySpace.setOnDragListener(new OnDragListener() {
			@Override
			public boolean onDrag(View v, DragEvent event) {
				switch (event.getAction()) {
				case DragEvent.ACTION_DRAG_STARTED:
					mSlidingMenu.showContent();
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
		mConsoleListView.addOnLayoutChangeListener(new OnLayoutChangeListener() {
			@Override
			public void onLayoutChange(View v, int left, int top, int right,
					int bottom, int oldLeft, int oldTop, int oldRight,
					int oldBottom) {
				mConsoleEntriesHeight = 0;
				mainloop: for (int groupPos = 0; groupPos < mConsoleEntries.size(); groupPos++) {
					View group = mConsoleListView.getExpandableListAdapter().getGroupView(groupPos, true, null, mConsoleListView);
					if (measureView(group)) {
						break mainloop;
					}

					int children = mConsoleListView.getExpandableListAdapter().getChildrenCount(groupPos);
					for (int childPos = 0; childPos < children; childPos++) {
						View child = mConsoleListView.getExpandableListAdapter().getChildView(groupPos, childPos, true, null, mConsoleListView);
						if (measureView(child)) {
							break mainloop;
						}
					}
				}
				resizeEmptySpace();
			}

			private boolean measureView(View v) {
				if (v.getLayoutParams() == null) {
					v.setLayoutParams(new LayoutParams(0, 0)); // Prevent crashing on CyanogenMod
				}
				v.measure(MeasureSpec.makeMeasureSpec(mConsoleWidth, MeasureSpec.AT_MOST),
						MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));
				mConsoleEntriesHeight += v.getMeasuredHeight();
				if (mConsoleEntriesHeight >= mScreenHeight) {
					mConsoleEntriesHeight = mScreenHeight;
					return true;
				} else {
					return false;
				}
			}
		});

		if (savedInstanceState == null) {
			mSearcher = new ConsoleWordSearcher(mConsoleListAdapter);
		} else {
			mSearcher = savedInstanceState.getParcelable("consoleSearcher");
			mSearcher.attachAdapter(mConsoleListAdapter);
		}

		mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistory);
		mCommandHistoryView.setAdapter(mCommandHistoryAdapter);
		mCommandHistoryView.setEmptyView(findViewById(R.id.command_history_empty_view));

		if (savedInstanceState != null) {
			mMenuSearchConstraint = savedInstanceState.getCharSequence("menuSearchConstraint");
			if (mMenuSearchConstraint != null) {
				mCommandExpandableMenuAdapter = new CommandExpandableMenuAdapter(this, mMenuSearchConstraint);
			} else {
				mCommandExpandableMenuAdapter = new CommandExpandableMenuAdapter(this);
			}
		} else {
			mCommandExpandableMenuAdapter = new CommandExpandableMenuAdapter(this);
		}
		mCommandExpandableMenuView.setAdapter(mCommandExpandableMenuAdapter);
		mCommandExpandableMenuView.setOnChildClickListener(new OnChildClickListener() {
			@Override
			public boolean onChildClick(ExpandableListView parent, View v, int groupPosition, int childPosition, long id) {
				String cmdName = mCommandExpandableMenuAdapter.getChild(groupPosition, childPosition);
				appendInputText(cmdName + StringUtils.NBSP);
				return true;
			}
		});
		mCommandExpandableMenuView.setOnItemLongClickListener(new OnExpandableItemLongClickListener() {
			@Override
			public boolean onChildLongClick(ExpandableListView parent, View v, int groupPosition, int childPosition, long id) {
				CommandExpandableMenuItem item = (CommandExpandableMenuItem) mCommandExpandableMenuAdapter.
						getChildView(groupPosition, childPosition, false, v, parent).getTag();
				String commandName = item.commandName.getText().toString();
				List<? extends CommandInfo> commandInfos = CommandHolder.getCommandsFromName(commandName);
				CommandHelpDialog helpDialog = CommandHelpDialog.newInstance(commandInfos);
				helpDialog.show(getFragmentManager(), "commandHelp");
				return true;
			}
		});

		//commandExpandableMenuSearch.setCompoundDrawable
		mCommandExpandableSearchView.setOnEditorActionListener(new OnEditorActionListener() {
			@Override
			public boolean onEditorAction(TextView v, int actionId,
					KeyEvent event) {
				if (event == null || event.getAction() == KeyEvent.ACTION_UP) {
					CharSequence constraint = mCommandExpandableSearchView.getText();
					mMenuSearchConstraint = constraint.length() == 0 ? null : constraint;
					mCommandExpandableMenuAdapter.getFilter().filter(mMenuSearchConstraint);
					mCommandExpandableSearchView.clearFocus();
					mConsoleInputEditText.requestFocus();
					return true;
				}
				return false;
			}
		});
		//		if (savedInstanceState != null) {
		//			mMenuSearchConstraint = savedInstanceState.getCharSequence("menuSearchConstraint");
		//			if (mMenuSearchConstraint != null) {
		//				mCommandExpandableMenuAdapter.getFilter().filter(mMenuSearchConstraint);
		//			}
		//		}

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
					@Override
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
							addUserInputEntry(input);
						} else {
							int size = mUserInputHistory.size();
							String trimput = input.trim();
							if (size == 1 || (size > 1 &&
									!mUserInputHistory.get(size - 2).equals(trimput))) {
								mUserInputHistory.set(size - 1, trimput);
								mUserInputHistory.add("");
								mUserInputHistoryChoice = size;
							} else {
								mUserInputHistoryChoice = size - 1;
							}
							float lineWidth = mConsoleInputEditText.getMeasuredWidth();
							float charWidth = mConsoleInputEditText.getPaint().measureText(" ");
							mHermitClient.runCommand(input, (int) (lineWidth / charWidth));
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

		nextEntryButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				if (mUserInputHistoryChoice < mUserInputHistory.size() - 1) {
					selectFromUserInputHistory(mUserInputHistoryChoice + 1);
				}
			}
		});

		scrollEntriesButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				if (mUserInputHistory.size() > 1) {
					showDialog(ScrollEntriesDialog.newInstance(mUserInputHistoryChoice, mUserInputHistory), SCROLL_ENTRIES_TAG);
				}
			}
		});

		prevEntryButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				if (mUserInputHistoryChoice > 0) {
					selectFromUserInputHistory(mUserInputHistoryChoice - 1);
				}
			}
		});

		toggleSpecialKeysButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				Prefs.setSpecialKeysVisible(ConsoleActivity.this, !Prefs.getSpecialKeysVisible(ConsoleActivity.this));
			}
		});

		hideOptionsBarButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mConsoleOptionsBar.setVisibility(View.GONE);
				invalidateOptionsMenu();
			}
		});

		final SpecialKeyAdapter ska = new SpecialKeyAdapter(this, Lists.newArrayList("|", "[", "]", "{", "}", "∀", "#", "→", "△", "▲", "λ"));
		ska.setTypeface(TYPEFACE);
		specialKeyRow.setAdapter(ska);
		specialKeyRow.post(new Runnable() {
			@Override
			public void run() {
				View charView = ska.getView(0, null, specialKeyRow);
				charView.measure(MeasureSpec.makeMeasureSpec(mConsoleWidth, MeasureSpec.AT_MOST),
						MeasureSpec.makeMeasureSpec(0, MeasureSpec.UNSPECIFIED));
				specialKeyRow.getLayoutParams().height = charView.getMeasuredHeight();
				specialKeyRow.requestLayout();
			}
		});

		updateConsoleEntries();
		resizeSlidingMenu();
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putParcelableArrayList("commandHistory", mCommandHistory);
		outState.putInt("consoleInputNum", mConsoleInputNum);
		outState.putString("consoleInput", mConsoleInputEditText.getText().toString());
		outState.putParcelable("consoleSearcher", mSearcher);
		outState.putInt("consoleInputCursor", mConsoleInputEditText.getSelectionStart());
		if (mSearchInputView != null) {
			outState.putString("searchMatches", mSearchMatches.getText().toString());
			outState.putString("searchInput", mSearchInputView.getText().toString());
			outState.putString("prevSearchCriterion", mPrevSearchCriterion);
		}
		outState.putCharSequence("menuSearchConstraint", mMenuSearchConstraint);
		outState.putBoolean("softKeyboardVisibility", mSoftKeyboardVisible);
		outState.putBoolean("findTextEnabled", mSearchEnabled);
		outState.putParcelableArrayList("consoleEntries", mConsoleEntries);
		outState.putParcelable("hermitClient", mHermitClient);
		outState.putStringArrayList("userInputHistory", mUserInputHistory);
		outState.putInt("userInputHistoryChoice", mUserInputHistoryChoice);
	}

	@Override
	public void onBackPressed() {
		exit(false);
	}

	@Override
	public void onActivityResult(int requestCode, int resultCode, Intent data) {
		switch (requestCode) {
		case BluetoothUtils.REQUEST_ENABLE_BLUETOOTH:
			if (mHermitClient.isRequestDelayed()) {
				if (resultCode == RESULT_OK) {
					mHermitClient.runDelayedRequest();
				} else {
					appendErrorResponse("ERROR: Failed to enable Bluetooth.");
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
					appendErrorResponse("ERROR: Failed to enable Wi-Fi.");
				}
			}
			break;
		case BluetoothUtils.REQUEST_FIND_BLUETOOTH_DEVICE:
			if (mHermitClient.isRequestDelayed()) {
				if (resultCode == RESULT_OK) {
					String name = data.getStringExtra(BluetoothDeviceListActivity.EXTRA_DEVICE_NAME);
					String address = data.getStringExtra(BluetoothDeviceListActivity.EXTRA_DEVICE_ADDRESS);
					Prefs.setBluetoothDeviceName(this, name);
					Prefs.setBluetoothDeviceAddress(this, address);
					mHermitClient.runDelayedRequest();
				} else {
					appendErrorResponse("ERROR: Failed to locate Bluetooth device.");
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
		menu.findItem(R.id.show_console_options_bar).setVisible(mConsoleOptionsBar.getVisibility() == View.GONE);

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
						String searchCriterion = StringUtils.charWrap(mSearchInputView.getText().toString());
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
		case android.R.id.home:
			exit(false);
			return true;
		case R.id.gestures:
			GestureDialog gd = new GestureDialog();
			gd.show(getFragmentManager(), "gesture");
			return true;
		case R.id.complete:
			mHermitClient.completeInput(getInput());
			return true;
		case R.id.find_text_option:
			mSearchEnabled = true;
			invalidateOptionsMenu();
			return true;
		case R.id.show_console_options_bar:
			mConsoleOptionsBar.setVisibility(View.VISIBLE);
			invalidateOptionsMenu();
			return true;
		case R.id.save_history:
			mHermitClient.fetchHistory();
			return true;
		case R.id.load_history:
			mHermitClient.loadHistory();
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
		switch (v.getId()) {
		case R.id.console_empty_space:
			super.onCreateContextMenu(menu, v, menuInfo);
			menu.setHeaderTitle("Input options");
			getMenuInflater().inflate(R.menu.console_empty_space_context_menu, menu);
			if (getInputLength() == 0) {
				menu.findItem(R.id.console_input_paste_append).setTitle("Paste");
				menu.findItem(R.id.console_input_paste_append).setTitleCondensed("Paste");
				menu.findItem(R.id.console_input_paste_replace).setVisible(false);
			}
			break;
		case R.id.console_list_view:
			if (!(menuInfo instanceof AdapterContextMenuInfo)) {
				ExpandableListContextMenuInfo expInfo = (ExpandableListContextMenuInfo) menuInfo;
				int groupPos = ExpandableListView.getPackedPositionGroup(expInfo.packedPosition);
				if (!mConsoleEntries.get(groupPos).getShortContents().toString().isEmpty()) { //To prevent empty lines
					super.onCreateContextMenu(menu, v, menuInfo);
					menu.setHeaderTitle("Entry " + groupPos + ": Commands found");
					menu.add(Menu.NONE, Menu.NONE, 1, "Sample transformation (does nothing)");
				}
			}
			break;
		}
	}

	@Override
	public boolean onContextItemSelected(MenuItem item) {
		if (item != null) {
			switch (item.getGroupId()) {
			case DRAGGED_GROUP:
				String keywordNStr = item.getTitle().toString();
				if (mInputEnabled) {
					CustomCommandDispatcher.runCustomCommand(this, mTempCommand, keywordNStr);
				} else {
					mConsoleInputEditText.setText(mTempCommand + StringUtils.NBSP + keywordNStr);
				}
				break;
			case LONG_CLICKED_GROUP:
				// Fill in with something useful later
				break;
			case R.id.console_input_group:
				switch (item.getItemId()) {
				case R.id.console_input_select:
					mConsoleInputEditText.setSelection(0, getInputLength());
					break;
				case R.id.console_input_copy: {
					ClipboardManager clipboard = (ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE);
					ClipData copiedText = ClipData.newPlainText("copiedText", getInput());
					clipboard.setPrimaryClip(copiedText);
					showToast("Selection copied to clipboard!");
					break;
				}
				case R.id.console_input_paste_append:
				case R.id.console_input_paste_replace: {
					ClipboardManager clipboard = (ClipboardManager) getSystemService(Context.CLIPBOARD_SERVICE);
					if (clipboard.hasPrimaryClip() && clipboard.getPrimaryClipDescription().hasMimeType("text/plain")) {
						CharSequence pasteData = clipboard.getPrimaryClip().getItemAt(0).getText();
						if (pasteData != null) {
							String pastedText = StringUtils.charWrap(pasteData.toString());
							if (item.getItemId() == R.id.console_input_paste_append) {
								mConsoleInputEditText.append(pastedText);
							} else {
								mConsoleInputEditText.setText(pastedText);
							}
							mConsoleInputEditText.setSelection(getInputLength());
						}
					}
					break;
				}
				}
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
			mHermitClient.completeInput(getInput());
			return true;
		}
		return super.onKeyDown(keyCode, event);
	}

	public void addCommandHistoryEntry(int fromAst, String command, int toAst) {
		mCommandHistory.add(new HistoryCommand(fromAst, command, toAst));
		mCommandHistoryAdapter.notifyDataSetChanged();
	}

	public void addUserInputEntry(String userInput) {
		addEntry(userInput, null, null);
	}

	public void addCommandResponseEntry(CommandResponse commandResponse) {
		addEntry(null, commandResponse, null);
	}

	public void addErrorResponseEntry(String errorResponse) {
		addEntry(null, null, errorResponse);
	}

	private void addEntry(String userInput, CommandResponse commandResponse, String errorResponse) {
		if (userInput != null) {
			mNextConsoleEntry.setUserInput(userInput);
		}
		if (commandResponse != null) {
			mNextConsoleEntry.appendCommandResponse(commandResponse);
		}
		if (errorResponse != null) {
			mNextConsoleEntry.appendErrorResponse(errorResponse);
		}

		mConsoleInputNum++;
		mConsoleEntries.add(mNextConsoleEntry);
		updateConsoleEntries();
		scrollToBottom();
	}

	public void clearCommandHistory() {
		mCommandHistory.clear();
		mCommandHistoryAdapter.notifyDataSetChanged();
	}

	public void removeConsoleEntry() {
		if (!mConsoleEntries.isEmpty()) {
			mConsoleInputNum--;
			mConsoleEntries.remove(getEntryCount() - 1);
			updateConsoleEntries();
			scrollToBottom();
		}
	}

	public void appendErrorResponse(String errorResponse) {
		if (!mConsoleEntries.isEmpty()) {
			ConsoleEntry prevEntry = mConsoleEntries.get(getEntryCount() - 1);
			if (prevEntry.getErrorResponse() == null) {
				prevEntry.appendErrorResponse(errorResponse);
				updateConsoleEntries();
				scrollToBottom();
			} else {
				addErrorResponseEntry(errorResponse);
			}
		} else {
			addErrorResponseEntry(errorResponse);
		}
	}

	public void appendCommandResponse(CommandResponse commandResponse) {
		if (!mConsoleEntries.isEmpty()) {
			ConsoleEntry prevEntry = mConsoleEntries.get(getEntryCount() - 1);
			if (prevEntry.getCommandResponse() == null) {
				prevEntry.appendCommandResponse(commandResponse);
				updateConsoleEntries();
				scrollToBottom();
			} else {
				addCommandResponseEntry(commandResponse);
			}
		} else {
			addCommandResponseEntry(commandResponse);
		}
	}

	public void appendInputText(String text) {
		mConsoleInputEditText.getText().append(text);
		mConsoleInputEditText.setSelection(getInputLength());
	}

	public void clear() {
		mConsoleInputNum = 0;
		mConsoleEntries.clear();
		updateConsoleEntries();
	}

	public void disableInput(boolean hideInput) {
		mInputEnabled = false;
		mConsoleInputLayout.setVisibility(hideInput ? View.INVISIBLE : View.VISIBLE);
	}

	public void enableInput() {
		mInputEnabled = true;
		mConsoleInputLayout.setVisibility(View.VISIBLE);
		mConsoleInputEditText.requestFocus();
	}

	public void exit(boolean forceExit) {
		if (forceExit) {
			exitForced();
		} else {
			String title = getResources().getString(R.string.console_exit_title);
			String message = getResources().getString(R.string.console_exit_message);
			YesOrNoDialog exitDialog = new YesOrNoDialog(title, message) {
				@Override
				protected void yes(DialogInterface dialog, int whichButton) {
					exitForced();
				}
			};
			exitDialog.show(getFragmentManager(), "exit");
		}
	}

	private void exitForced() {
		if (Prefs.getNetworkSource(this).equals(NetworkSource.BLUETOOTH_SERVER)) {
			if (BluetoothUtils.isBluetoothConnected(this)) {
				BluetoothUtils.closeBluetooth();
			}
		}
		
		((BaseApplication) getApplication()).cancelTasks(this);
		Intent intent = new Intent(this, MainActivity.class);
		intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);
		startActivity(intent);
	}

	public ConsoleEntry getEntry(int index) {
		return mConsoleEntries.get(index);
	}

	public CharSequence getLine(int entryIndex, int lineIndex) {
		return getEntry(entryIndex).getContentLines().get(lineIndex);
	}

	/**
	 * @return the total number of console entries.
	 */
	public int getEntryCount() {
		return mConsoleEntries.size();
	}

	public HermitClient getHermitClient() {
		return mHermitClient;
	}

	public String getInput() {
		return StringUtils.noCharWrap(mConsoleInputEditText.getText().toString());
	}

	public int getInputLength() {
		return mConsoleInputEditText.length();
	}

	/**
	 * @return the entry number for the console input field.
	 */
	public int getInputNum() {
		return mConsoleInputNum;
	}

	public boolean isSoftKeyboardVisible() {
		return mSoftKeyboardVisible;
	}

	public void selectFromUserInputHistory(int choice) {
		mUserInputHistoryChoice = choice;
		mConsoleInputEditText.setText(mUserInputHistory.get(mUserInputHistoryChoice));
		mConsoleInputEditText.setSelection(getInputLength());
		mConsoleInputEditText.requestFocus();
	}

	public void setInputEnabled(boolean enabled) {
		mInputEnabled = enabled;
	}

	public void setInputText(String text) {
		setInputText(0, getInputLength(), text);
	}

	public void setInputText(int start, int end, String text) {
		mConsoleInputEditText.getText().replace(start, end, text);
		mConsoleInputEditText.setSelection(getInputLength());
	}

	public void showEntrySelectionDialog(SortedSet<ConsoleLineParams> lineParams) {
		showDialog(ConsoleEntrySelectionDialog.newInstance(lineParams), SELECTION_TAG);
	}

	public void showKeywordSwapDialog(int entryNum, String entryContents) {
		showDialog(KeywordSwapDialog.newInstance(entryNum, entryContents), KEYWORD_SWAP_TAG);
	}

	public void showInputCompletionDialog(int replaceIndex, Collection<String> suggestions) {
		if (suggestions.size() > 0) {
			showDialog(InputCompletionDialog.newInstance(replaceIndex, suggestions), WORD_COMPLETION_TAG);
		}
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

	public void attemptInputCompletion(Collection<String> existingSuggestions) {
		SortedSet<String> suggestions = new TreeSet<String>();
		if (existingSuggestions != null) {
			suggestions.addAll(existingSuggestions);
		}

		final String input = mConsoleInputEditText.getText().toString();
		int replaceIndex = Math.max(0, input.length() - 1);
		if (input.length() > 0 &&
				!String.valueOf(input.charAt(replaceIndex)).matches(StringUtils.WHITESPACE)) {
			replaceIndex = StringUtils.findLastWordIndex(input);
		}
		if (replaceIndex == StringUtils.findFirstWordIndex(input)
				|| input.isEmpty() || input.matches(StringUtils.WHITESPACE)) {
			final String trimput = StringUtils.trim(input);
			suggestions.addAll(Collections2.filter(CustomCommandDispatcher.getCommandSet(),
					new Predicate<String>() {
				@Override
				public boolean apply(String suggestion) {
					return suggestion.startsWith(trimput);
				}
			}));
		}

		if (suggestions.size() == 1) {
			for (String completion : suggestions) {
				completeInput(replaceIndex, completion);
				break;
			}
		} else if (suggestions.size() > 1) {
			showInputCompletionDialog(replaceIndex, suggestions);
		}
	}

	public void completeInput(int replaceIndex, String completion) {
		String input = getInput();
		if (input.length() > 0 &&
				String.valueOf(input.charAt(input.length() - 1)).matches(StringUtils.WHITESPACE)) {
			completion = StringUtils.NBSP + completion;
		}
		setInputText(replaceIndex, getInputLength(), completion + StringUtils.NBSP);
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

			//			if (params != null && !Views.isEntryVisible(mConsoleListView, params.listIndex)) {
			//				mConsoleListView.smoothScrollToPositionFromTop(params.listIndex,
			//						params.textViewOffset, SCROLL_DURATION);
			//			}

			if (params != null) {
				int flatListPos = Views.getFlatListPosition(mConsoleListView, params.entryIndex, params.lineIndex);
				if (!Views.isEntryVisible(mConsoleListView, flatListPos)) {
					mConsoleListView.smoothScrollToPositionFromTop(flatListPos,
							params.textViewOffset, SCROLL_DURATION);
				}
			}
			setTextSearchCaption();
			setInputEnabled(true);
		}
	}

	private synchronized void resizeEmptySpace() {
		mConsoleEmptySpace.post(new Runnable() {
			@Override
			public void run() {
				mConsoleEmptySpace.getLayoutParams().height = mScreenHeight - mConsoleEntriesHeight - mConsoleInputHeight;
				mConsoleEmptySpace.requestLayout();
			}
		});
	}

	/**
	 * Changes the SlidingMenu width depending on the screen size.
	 */
	private void resizeSlidingMenu() {
		int iconWidth = getResources().getDrawable(R.drawable.template_white).getIntrinsicWidth();
		int plusWidth = getResources().getDrawable(R.drawable.ic_plus_light).getIntrinsicWidth();
		int totalWidth = iconWidth + plusWidth;
		mSlidingMenu.setBehindWidth(Math.round(totalWidth*1.05f));
	}

	/**
	 * Show the entry at the bottom of the console ListView.
	 */
	private void scrollToBottom() {
		mConsoleListView.post(new Runnable() {
			@Override
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

	void setCommandHistory(List<HistoryCommand> historyCommands) {
		mCommandHistory = new ArrayList<HistoryCommand>(historyCommands);
		Collections.sort(mCommandHistory);
		mCommandHistoryAdapter = new CommandHistoryAdapter(this, mCommandHistory);
		mCommandHistoryView.setAdapter(mCommandHistoryAdapter);
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
	void updateConsoleEntries() {
		if (mConsoleEntries.size() > CONSOLE_ENTRY_LIMIT) {
			mConsoleEntries.remove(0);
		}
		mConsoleListView.expandAllGroups();
		mConsoleListAdapter.notifyDataSetChanged();
		updateInput();
	}

	void updateCommandExpandableMenu() {
		int count = mCommandExpandableMenuAdapter.getGroupCount();
		for (int i = 0; i < count; i++) {
			mCommandExpandableMenuView.collapseGroup(i);
		}
		mCommandExpandableMenuAdapter.notifyDataSetChanged();
	}

	void updateInput() {
		mNextConsoleEntry = new ConsoleEntry(getInputNum(), mHermitClient.getAst(), null);
		mConsoleInputNumView.setText(mNextConsoleEntry.getFullContentsPrefix());
		mConsoleInputNumView.measure(MeasureSpec.UNSPECIFIED, MeasureSpec.UNSPECIFIED);
		final int width = mConsoleInputNumView.getMeasuredWidth();
		final int padding = mConsoleInputNumView.getPaddingLeft();
		mConsoleInputEditText.setIndent(width - padding);
	}

	private enum SearchAction { BEGIN, CONTINUE, RESUME, END }

}
