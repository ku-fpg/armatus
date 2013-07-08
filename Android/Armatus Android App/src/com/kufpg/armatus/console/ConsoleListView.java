package com.kufpg.armatus.console;

import android.content.Context;
import android.os.SystemClock;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.AdapterView.OnItemLongClickListener;
import android.widget.ListView;

public class ConsoleListView extends ListView implements OnItemClickListener, OnItemLongClickListener {
	private ConsoleActivity mConsole;
	private boolean mLongClicking = false;
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
		setOnItemLongClickListener(this);
	}

	@Override
	public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
		if (mLongClicking) {
			mLongClicking = false;
			setItemChecked(mPrevCheckedPos, true);
		} else {
			boolean showActionBar = true;
			if (mPrevCheckedPos == getCheckedItemPosition()) {
				showActionBar = false;
			}
			mConsole.setContextualActionBarVisible(showActionBar);
			mPrevCheckedPos = getCheckedItemPosition();
		}
	}

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
		mLongClicking = true;
		return false;
	}

	public void stopScroll() {
		MotionEvent cancel = MotionEvent.obtain(SystemClock.uptimeMillis(),
				SystemClock.uptimeMillis(),	MotionEvent.ACTION_CANCEL, 0, 0, 0);
		dispatchTouchEvent(cancel);
		cancel.recycle();
	}

	void setPrevCheckedPos(int pos) {
		mPrevCheckedPos = pos;
	}

}
