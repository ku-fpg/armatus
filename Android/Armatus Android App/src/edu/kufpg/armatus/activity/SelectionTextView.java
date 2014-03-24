package edu.kufpg.armatus.activity;

import java.util.ArrayList;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.TextView;

public class SelectionTextView extends TextView {

	private ArrayList<SelectionWatcher> mSelectionWatchers;

	public SelectionTextView(Context context) {
		super(context);
	}

	public SelectionTextView(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public SelectionTextView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public void addSelectionWatcher(SelectionWatcher watcher) {
		if (mSelectionWatchers == null) {
			mSelectionWatchers = new ArrayList<SelectionWatcher>();
		}
		mSelectionWatchers.add(watcher);
	}

	public void removeSelectionWatcher(SelectionWatcher watcher) {
		if (mSelectionWatchers != null) {
			int i = mSelectionWatchers.indexOf(watcher);
			if (i >= 0) {
				mSelectionWatchers.remove(i);
			}
		}
	}

	@Override
	protected void onSelectionChanged(int selStart, int selEnd) {
		if (mSelectionWatchers != null) {
			for (SelectionWatcher watcher : mSelectionWatchers) {
				watcher.onSelectionChanged(selStart, selEnd);
			}
		}
		super.onSelectionChanged(selStart, selEnd);
	}

	public static interface SelectionWatcher {
		void onSelectionChanged(int selStart, int selEnd);
	}

}
