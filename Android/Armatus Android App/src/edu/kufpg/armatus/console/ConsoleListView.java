package edu.kufpg.armatus.console;

import java.util.SortedSet;
import java.util.TreeSet;

import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
import android.content.Intent;
import android.os.Parcel;
import android.os.Parcelable;
import android.util.AttributeSet;
import android.view.ActionMode;
import android.view.ActionMode.Callback;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.ExpandableListAdapter;
import android.widget.ExpandableListView;
import android.widget.ListView;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.activity.ConsoleEntryIntent;
import edu.kufpg.armatus.activity.ConsoleEntryScopeActivity;
import edu.kufpg.armatus.activity.ConsoleEntrySelectionActivity;
import edu.kufpg.armatus.activity.ConsoleEntrySelectionActivity2;
import edu.kufpg.armatus.util.ParcelUtils;
import edu.kufpg.armatus.util.StringUtils;
import edu.kufpg.armatus.util.Views;

public class ConsoleListView extends ExpandableListView {
	private ConsoleActivity mConsole;
	private ActionMode mActionMode;
	private ConsoleListViewCallback2 mActionModeCallback;
	private boolean mActionModeVisible = false;
	private SortedSet<Integer> mPrevCheckedStates = new TreeSet<Integer>();

	public ConsoleListView(Context context) {
		super(context);
		init(context);
	}

	public ConsoleListView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init(context);
	}

	public ConsoleListView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init(context);
	}

	private void init(Context context) {
		mConsole = (ConsoleActivity) context;
		mActionModeCallback = new ConsoleListViewCallback2(mConsole, this);
		setGroupIndicator(null);
		setOnGroupClickListener(new OnGroupClickListener() { // Disable collapsing of groups
			@Override
			public boolean onGroupClick(ExpandableListView parent, View v, int groupPosition, long id) {
				return true;
			}
		});
		setOnChildClickListener(new OnChildClickListener() {
			@Override
			public boolean onChildClick(ExpandableListView parent, View v, int groupPosition, int childPosition, long id) {
				boolean showActionMode = true;
				int combinedChildId = Views.getFlatListPosition(ConsoleListView.this, groupPosition, childPosition);
				//int combinedChildId = getFlatListPosition(getPackedPositionForChild(groupPosition, childPosition));

				setItemChecked(combinedChildId, !mPrevCheckedStates.contains(combinedChildId));
				if (mPrevCheckedStates.contains(combinedChildId)) {
					mPrevCheckedStates.remove(combinedChildId);
				} else {
					mPrevCheckedStates.add(combinedChildId);
				}

				if (mPrevCheckedStates.isEmpty()) {
					showActionMode = false;
				}
				setActionModeVisible(showActionMode);
				refreshActionMode();
				return true;
			}
		});
	}

	public boolean isActionModeVisible() {
		return mActionModeVisible;
	}

	/**
	 * Shows or hides the {@link ListView}'s {@link ActionMode}.
	 * @param visible {@code true} if the {@code ActionMode} should be shown,
	 * {@code false} if it should be hidden.
	 */
	public void setActionModeVisible(boolean visible) {
		if (visible && !mActionModeVisible) {
			mActionMode = startActionMode(mActionModeCallback);
		} else if (!visible && mActionModeVisible) {
			mActionMode.finish();
		}
	}

	private void refreshActionMode() {
		if (isActionModeVisible()) {
			updateSinglyClickedActionModeItems(mPrevCheckedStates.size());
		}
	}

	@Override
	public Parcelable onSaveInstanceState() {
		Parcelable superState = super.onSaveInstanceState();
		SavedState ss = new SavedState(superState);
		ss.checkedStates = mPrevCheckedStates;
		return ss;
	}

	@Override
	public void onRestoreInstanceState(Parcelable state) {
		if (!(state instanceof SavedState)) {
			super.onRestoreInstanceState(state);
		}

		SavedState ss = (SavedState) state;
		super.onRestoreInstanceState(ss.getSuperState());
		mPrevCheckedStates = ss.checkedStates;
		if (!mPrevCheckedStates.isEmpty()) {
			setActionModeVisible(true);
			for (int item : mPrevCheckedStates) {
				setItemChecked(item, true);
			}
		}
		refreshActionMode();
	}

	protected SortedSet<Integer> getPrevCheckedStates() {
		return mPrevCheckedStates;
	}

	void setActionModeVisibleInternal(boolean visible) {
		mActionModeVisible = visible;
	}

	void updateSinglyClickedActionModeItems(int itemsSelected) {
		boolean oneSelected = itemsSelected == 1;
		mActionMode.setSubtitle(oneSelected ? "One entry selected" : itemsSelected + " entries selected");
		mActionModeCallback.setSinglyClickedItemVisibility(oneSelected);
	}

	@Override
	public void setAdapter(ExpandableListAdapter adapter) {
		super.setAdapter(adapter);
		expandAllGroups();
	}

	public void expandAllGroups() {
		for (int i = 0; i < getExpandableListAdapter().getGroupCount(); i++) {
			expandGroup(i);
		}
	}

	protected static class ConsoleListViewCallback2 implements Callback {
		private ConsoleActivity mConsole;
		private ConsoleListView mListView;
		private MenuItem mSwapItem, mTransformItem, mTransform2Item;

		public ConsoleListViewCallback2(ConsoleActivity console, ConsoleListView listView) {
			mConsole = console;
			mListView = listView;
		}

		@Override
		public boolean onCreateActionMode(ActionMode mode, Menu menu) {
			MenuInflater inflater = mode.getMenuInflater();
			inflater.inflate(R.menu.console_list_view_action_mode, menu);
			mode.setTitle("Select entries");
			mSwapItem = menu.findItem(R.id.console_list_view_swap);
			mTransformItem = menu.findItem(R.id.console_list_view_transform);
			mTransform2Item = menu.findItem(R.id.console_list_view_transform2);
			return true;
		}

		@Override
		public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
			mListView.setActionModeVisibleInternal(true);
			return true;
		}

		@Override
		public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
			final SortedSet<Integer> prevCheckedStates = mListView.getPrevCheckedStates();

			switch (item.getItemId()) {
			case R.id.console_list_view_copy:
				StringBuilder copyBuilder = new StringBuilder();
				for (int state : prevCheckedStates) {
					copyBuilder.append(((ConsoleEntry) mListView.getItemAtPosition(state))
							.getFullContents()).append('\n');
				}
				copyBuilder.deleteCharAt(copyBuilder.length() - 1); //Remove final newline
				ClipboardManager clipboard = (ClipboardManager) mConsole.getSystemService(Context.CLIPBOARD_SERVICE);
				ClipData copiedText = ClipData.newPlainText("copiedText",
						StringUtils.noCharWrap(copyBuilder.toString()));
				clipboard.setPrimaryClip(copiedText);
				mConsole.showToast((prevCheckedStates.size() == 1 ? "Entry" : "Entries") + " copied to clipboard!");
				mode.finish();
				return true;
			case R.id.console_list_view_select:
				SortedSet<ConsoleLineParams> lineParams = new TreeSet<ConsoleLineParams>();
				for (int state : prevCheckedStates) {
					int groupPos = Views.getGroupPosition(mListView, state);
					int childPos = Views.getChildPosition(mListView, state);
					lineParams.add(new ConsoleLineParams(groupPos, childPos));
				}
				mConsole.showEntrySelectionDialog(lineParams);
				mode.finish();
				return true;
			case R.id.console_list_view_swap:
				if (prevCheckedStates.size() == 1) {
					int selState = prevCheckedStates.first();
					int groupPos = Views.getGroupPosition(mListView, selState);
					ConsoleEntry entry = mConsole.getEntry(groupPos);
					if (entry.getShortContents().toString().split(StringUtils.WHITESPACE).length > 1) {
						mConsole.showKeywordSwapDialog(entry.getEntryNum(), entry.getShortContents().toString());
					}
					mode.finish();
				}
				return true;
			case R.id.console_list_view_transform:
				if (prevCheckedStates.size() == 1) {
					int selState = prevCheckedStates.first();
					int groupPos = Views.getGroupPosition(mListView, selState);
					ConsoleEntry entry = mConsole.getEntry(groupPos);
					if (entry.getCommandResponse() != null && entry.getCommandResponse().getGlyphs() != null) {
						Intent intent = new ConsoleEntryIntent(entry, mConsole, ConsoleEntrySelectionActivity.class);
						mConsole.startActivity(intent);
					}
					mode.finish();
				}
				return true;
			case R.id.console_list_view_transform2:
				if (prevCheckedStates.size() == 1) {
					int selState = prevCheckedStates.first();
					int groupPos = Views.getGroupPosition(mListView, selState);
					ConsoleEntry entry = mConsole.getEntry(groupPos);
					if (entry.getCommandResponse() != null && entry.getCommandResponse().getGlyphs() != null) {
						Intent intent = new ConsoleEntryIntent(entry, mConsole, ConsoleEntrySelectionActivity2.class);
						mConsole.startActivity(intent);
					}
					mode.finish();
				}
				return true;
			case R.id.console_list_view_scope: {
				if (prevCheckedStates.size() == 1) {
					int selState = prevCheckedStates.first();
					int groupPos = Views.getGroupPosition(mListView, selState);
					ConsoleEntry entry = mConsole.getEntry(groupPos);
					Intent scopeIntent = new ConsoleEntryIntent(entry, mConsole, ConsoleEntryScopeActivity.class);
					mConsole.startActivity(scopeIntent);
					mode.finish();
				}
				return true;
			}
			}
			return false;
		}

		@Override
		public void onDestroyActionMode(ActionMode mode) {
			mListView.clearChoices();
			mListView.requestLayout();
			mListView.getPrevCheckedStates().clear();
			mListView.setActionModeVisibleInternal(false);
		}

		public void setSinglyClickedItemVisibility(boolean visible) {
			mSwapItem.setVisible(visible);
			mTransformItem.setVisible(visible);
			mTransform2Item.setVisible(visible);
		}

	}

	protected static class SavedState extends BaseSavedState {
		SortedSet<Integer> checkedStates;

		SavedState(Parcelable superState) {
			super(superState);
		}

		@Override
		public void writeToParcel(Parcel dest, int flags) {
			super.writeToParcel(dest, flags);
			ParcelUtils.writeCollection(dest, checkedStates);
		}

		public static final Parcelable.Creator<SavedState> CREATOR
		= new Parcelable.Creator<SavedState>() {
			public SavedState createFromParcel(Parcel in) {
				return new SavedState(in);
			}

			public SavedState[] newArray(int size) {
				return new SavedState[size];
			}
		};

		private SavedState(Parcel in) {
			super(in);
			checkedStates = ParcelUtils.readSortedSet(in);
		}
	}

}
