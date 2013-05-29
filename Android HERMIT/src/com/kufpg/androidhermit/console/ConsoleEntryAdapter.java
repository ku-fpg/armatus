package com.kufpg.androidhermit.console;

import java.util.List;

import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.drag.DragIcon;
import com.kufpg.androidhermit.drag.DragSinkListener;

import android.graphics.Typeface;
import android.view.ActionMode;
import android.view.DragEvent;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnLongClickListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

/**
 * ListView adapter used in conjunction with ConsoleActivity. This initializes the values of
 * each ConsoleEntry and defines event-driven behavior for everything in the ListView
 * (except for the footer).
 */
public class ConsoleEntryAdapter extends ArrayAdapter<ConsoleEntry> {
	public static final int HIGHLIGHTED = R.drawable.console_entry_highlighted;
	public static final int SELECTED = android.R.color.holo_blue_light;
	public static final int HIGHLIGHTED_SELECTED = R.drawable.console_entry_highlighted_selected;
	public static final int TRANSPARENT = android.R.color.transparent;

	private ConsoleActivity mConsole;
	private ListView mListView;
	private List<ConsoleEntry> mEntries;
	private Typeface mTypeface;
	private ConsoleEntryHolder mHolder;
	private ActionMode mActionMode;
	private int mCheckedPos = -1;

	public ConsoleEntryAdapter(ConsoleActivity console, List<ConsoleEntry> entries) {
		super(console, R.layout.console_entry, entries);
		mConsole = console;
		mListView = mConsole.getListView();
		mEntries = entries;
		mTypeface = Typeface.createFromAsset(mConsole.getResources().getAssets(),
				ConsoleActivity.TYPEFACE);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View entryView = convertView;
		if (entryView == null) { //If this is a new ConsoleEntry
			LayoutInflater inflater = mConsole.getLayoutInflater();
			entryView = inflater.inflate(R.layout.console_entry, parent, false);
			mHolder = new ConsoleEntryHolder();
			mHolder.num = (TextView) entryView.findViewById(R.id.console_entry_num);
			mHolder.contents = (TextView) entryView.findViewById(R.id.console_entry_contents);
			mHolder.loader = (RelativeLayout) entryView.findViewById(R.id.console_loading);
			entryView.setTag(mHolder);
		} else {
			mHolder = (ConsoleEntryHolder) entryView.getTag();
		}

		mHolder.num.setText("hermit<" + mEntries.get(position).getNum() + "> ");
		mHolder.num.setTypeface(mTypeface);

		PrettyPrinter.setPrettyText(mHolder.contents,
				mEntries.get(position).getContents());
		mHolder.contents.setTypeface(mTypeface);
		
		if (!mEntries.get(position).isWaiting()) {
			mHolder.loader.setVisibility(View.GONE);
		} else {
			mHolder.loader.setVisibility(View.VISIBLE);
		}

		final int thepos = position;

		entryView.setOnDragListener(new DragSinkListener() {
			@Override
			public void onDragEntered(View dragView, View dragSink, DragEvent event) {
				dragSink.setBackgroundResource(HIGHLIGHTED);
			}

			@Override
			public void onDragExited(View dragView, View dragSink) {
				dragSink.setBackgroundResource(TRANSPARENT);
			}

			@Override
			public void onDragEnded(View dragView, View dragSink) {
				dragSink.setBackgroundResource(TRANSPARENT);
			}

			@Override
			public void onDragDropped(View dragView, View dragSink) {
				List<String> keywords = mEntries.get(thepos).getKeywords();
				if (!keywords.isEmpty()) {
					mConsole.setTempCommand(((DragIcon) dragView).getCommandName());
					mConsole.openContextMenu(dragSink);
				}
			}
		});
		entryView.setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				if (!mEntries.get(thepos).getContents().isEmpty()) {
					mConsole.openContextMenu(v);
				}
				return true;
			}
		});
		entryView.setOnTouchListener(new OnTouchListener() {
			@Override
			public boolean onTouch(View v, MotionEvent event) {
				int mTouchedPos = mListView.getPositionForView(v);
				if (event.getAction() == MotionEvent.ACTION_DOWN) {
					if (mTouchedPos != mCheckedPos
							|| mCheckedPos == -1) {
						v.setBackgroundResource(HIGHLIGHTED);
					} else {
						v.setBackgroundResource(HIGHLIGHTED_SELECTED);
					}
				} else if (event.getAction() == MotionEvent.ACTION_UP
						|| event.getAction() == MotionEvent.ACTION_OUTSIDE
						|| event.getAction() == MotionEvent.ACTION_CANCEL) {
					if (mTouchedPos != mCheckedPos) {
						v.setBackgroundResource(TRANSPARENT);
					} else {
						v.setBackgroundResource(SELECTED);
					}
				}
				return false;
			}
		});
		entryView.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				int clickedPos = mListView.getPositionForView(v);
				if (mCheckedPos != clickedPos) {
					if (mCheckedPos != -1) {
						mListView.getChildAt(mCheckedPos).setBackgroundResource(TRANSPARENT);
					}
					selectEntry(clickedPos);
					mCheckedPos = mListView.getCheckedItemPosition();
				} else {
					hideActionBar();
				}
			}
		});

		return entryView;
	}

	/**
	 * Helper class that stores the views displaying the data of a ConsoleEntry.
	 * This is supposed to improve performance, if StackOverflow is to be believed.
	 */
	static class ConsoleEntryHolder {
		public TextView num;
		public TextView contents;
		public RelativeLayout loader;
	}

	public int getCheckedPos() {
		return mCheckedPos;
	}

	/**
	 * Sets an entry's value as checked and highlights it.
	 * @param pos The position of the entry to select.
	 */
	public void selectEntry(int pos) {
		mListView.setItemChecked(pos, true);
		mListView.getChildAt(pos).setBackgroundResource(SELECTED);
		showActionBar(pos);
	}

	/**
	 * Removes an entry's checked value and restores the default background color.
	 * Calling hideActionBar() will call this method.
	 * @param pos The position of the entry to deselect.
	 */
	public void deselectEntry() {
		if (mCheckedPos != -1) {
			mListView.getChildAt(mCheckedPos).setBackgroundResource(TRANSPARENT);
			mListView.setItemChecked(mCheckedPos, false);
			mCheckedPos = -1;
		}
	}

	/**
	 * Note: this gets called in selectEntry()
	 * @param checkedPos
	 */
	private void showActionBar(int checkedPos) {
		mActionMode = mConsole.startActionMode(new ConsoleEntryCallback(mConsole, this,
				getItem(checkedPos).getNum(), getItem(checkedPos).getContents()));
	}

	private void hideActionBar() {
		if (mActionMode != null) {
			mActionMode.finish();
			mActionMode = null;
		}
	}

}
