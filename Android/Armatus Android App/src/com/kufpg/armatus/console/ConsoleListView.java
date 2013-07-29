package com.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;

import com.kufpg.armatus.R;
import com.kufpg.armatus.util.ParcelableSparseBooleanArray;
import com.kufpg.armatus.util.StringUtils;

import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;
import android.util.AttributeSet;
import android.util.SparseBooleanArray;
import android.view.ActionMode;
import android.view.ActionMode.Callback;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListView;

public class ConsoleListView extends ListView {
	private ConsoleActivity mConsole;
	private ActionMode mActionMode;
	private ActionModeCallback mCallback;
	private MenuItem mSwapItem;
	private SparseBooleanArray mPrevCheckedStates = new ParcelableSparseBooleanArray();
	private int mPrevCheckedPos = INVALID_POSITION;
	private boolean mActionModeVisible = false;

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
		mCallback = new ActionModeCallback();
		setOnItemClickListener(new OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				if (position != getCount() - 1) {
					boolean showActionMode = true;
					if (mPrevCheckedStates.get(position) == true) {
						mPrevCheckedStates.delete(position);
					} else {
						mPrevCheckedStates.put(position, true);
					}
					if (mPrevCheckedStates.size() == 0) {
						showActionMode = false;
					}
					setActionModeVisible(showActionMode);

					if (mActionModeVisible) {
						switch (mPrevCheckedStates.size()) {
						case 1:
							mActionMode.setSubtitle("One entry selected");
							mSwapItem.setVisible(true);
							break;
						default:
							mActionMode.setSubtitle(mPrevCheckedStates.size() + " entries selected");
							mSwapItem.setVisible(false);
							break;
						}
					}
				}
			}
		});
	}

	@Override
	public Parcelable onSaveInstanceState() {
		Parcelable superState = super.onSaveInstanceState();
		SavedState ss = new SavedState(superState);
		ss.checkedStates = (ParcelableSparseBooleanArray) mPrevCheckedStates;
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
		setItemChecked(mPrevCheckedPos, true);
		if (mPrevCheckedStates.size() > 0) {
			setActionModeVisible(true);
			for (int i = 0; i < mPrevCheckedStates.size(); i++) {
				setItemChecked(mPrevCheckedStates.keyAt(i), true);
			}
		}
	}

	public void setActionModeVisible(boolean visible) {
		if (visible && !mActionModeVisible) {
			mActionMode = startActionMode(mCallback);
		} else if (!visible && mActionModeVisible) {
			mActionMode.finish();
		}
	}

	public boolean isEntryVisible(int entryIndex) {
		return getFirstVisiblePosition() <= entryIndex && entryIndex <= getLastVisiblePosition();
	}

	public boolean isActionModeVisible() {
		return mActionModeVisible;
	}

	private class ActionModeCallback implements Callback {

		@Override
		public boolean onCreateActionMode(ActionMode mode, Menu menu) {
			MenuInflater inflater = mode.getMenuInflater();
			inflater.inflate(R.menu.contextual_action_bar, menu);
			mode.setTitle("Select entries");
			mSwapItem = menu.findItem(R.id.swap);
			return true;
		}

		@Override
		public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
			mActionModeVisible = true;
			return true;
		}

		@Override
		public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
			switch (item.getItemId()) {
			case R.id.copy:
				StringBuilder copyBuilder = new StringBuilder();
				for (int i = 0; i < mPrevCheckedStates.size(); i++) {
					copyBuilder.append(((ConsoleEntry) getItemAtPosition(mPrevCheckedStates.keyAt(i)))
							.getFullContents()).append('\n');
				}
				copyBuilder.deleteCharAt(copyBuilder.length() - 1); //Remove final newline
				ClipboardManager clipboard = (ClipboardManager) mConsole.getSystemService(Context.CLIPBOARD_SERVICE);
				ClipData copiedText = ClipData.newPlainText("copiedText",
						StringUtils.removeCharWrap(copyBuilder.toString()));
				clipboard.setPrimaryClip(copiedText);
				mConsole.showToast((mPrevCheckedStates.size() == 1 ? "Entry" : "Entries") + " copied to clipboard!");
				mode.finish();
				return true;
			case R.id.select:
				List<ConsoleEntry> checkedEntries = new ArrayList<ConsoleEntry>();
				for (int i = 0; i < mPrevCheckedStates.size(); i++) {
					checkedEntries.add((ConsoleEntry) getItemAtPosition(mPrevCheckedStates.keyAt(i)));
				}
				mConsole.showEntrySelectionDialog(checkedEntries);
				mode.finish();
				return true;
			case R.id.swap:
				if (mPrevCheckedStates.size() == 1) {
					ConsoleEntry entry = (ConsoleEntry) getItemAtPosition(mPrevCheckedStates.keyAt(0));
					if (entry.getShortContents().split(StringUtils.WHITESPACE).length > 1) {
						mConsole.showKeywordSwapDialog(entry.getNum(), entry.getShortContents());
					}
					mode.finish();
				}
				return true;
			}
			return false;
		}

		@Override
		public void onDestroyActionMode(ActionMode mode) {
			clearChoices();
			requestLayout();
			mPrevCheckedStates.clear();
			mActionModeVisible = false;
		}

	}

	protected static class SavedState extends BaseSavedState {
		ParcelableSparseBooleanArray checkedStates;

		SavedState(Parcelable superState) {
			super(superState);
		}

		@Override
		public void writeToParcel(Parcel dest, int flags) {
			super.writeToParcel(dest, flags);
			dest.writeParcelable(checkedStates, flags);
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
			checkedStates = in.readParcelable(ConsoleListView.class.getClassLoader());
		}
	}

}
