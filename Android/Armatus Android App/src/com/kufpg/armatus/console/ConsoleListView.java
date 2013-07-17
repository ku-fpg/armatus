package com.kufpg.armatus.console;

import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;
import android.util.AttributeSet;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ListView;

public class ConsoleListView extends ListView implements OnItemClickListener {
	private ConsoleActivity mConsole;
	private int mPrevCheckedPos = INVALID_POSITION;

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
		setOnItemClickListener(this);
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
		boolean showActionBar = true;
		if (mPrevCheckedPos == getCheckedItemPosition()) {
			showActionBar = false;
		}
		mConsole.setContextualActionBarVisible(showActionBar);
		mPrevCheckedPos = getCheckedItemPosition();
		if (!showActionBar) {
			mPrevCheckedPos = INVALID_POSITION;
		}
	}
	
	@Override
	public Parcelable onSaveInstanceState() {
		Parcelable superState = super.onSaveInstanceState();
		SavedState ss = new SavedState(superState);
		ss.checkedPos = mPrevCheckedPos;
		return ss;
	}
	
	@Override
	public void onRestoreInstanceState(Parcelable state) {
		if (!(state instanceof SavedState)) {
			super.onRestoreInstanceState(state);
		}
		
		SavedState ss = (SavedState) state;
		super.onRestoreInstanceState(ss.getSuperState());
		mPrevCheckedPos = ss.checkedPos;
		setItemChecked(mPrevCheckedPos, true);
		if (mPrevCheckedPos != INVALID_POSITION) {
			mConsole.setContextualActionBarVisible(true);
		}
	}
	
	public boolean isEntryVisible(int entryIndex) {
		return getFirstVisiblePosition() <= entryIndex && entryIndex <= getLastVisiblePosition();
	}
	
	public void resetCheckedPos() {
		clearChoices();
		requestLayout();
		mPrevCheckedPos = INVALID_POSITION;
	}
	
	protected static class SavedState extends BaseSavedState {
		int checkedPos;

		SavedState(Parcelable superState) {
			super(superState);
		}

		@Override
		public void writeToParcel(Parcel dest, int flags) {
			super.writeToParcel(dest, flags);
			dest.writeInt(checkedPos);
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
			checkedPos = in.readInt();
		}
	}

}
