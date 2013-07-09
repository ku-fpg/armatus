package com.kufpg.armatus.console;

import android.content.Context;
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
	
	public void resetCheckedPos() {
		clearChoices();
		requestLayout();
		mPrevCheckedPos = INVALID_POSITION;
	}

}
