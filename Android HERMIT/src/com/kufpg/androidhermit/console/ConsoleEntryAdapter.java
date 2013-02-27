package com.kufpg.androidhermit.console;

import java.util.List;

import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.drag.DragIcon;
import com.kufpg.androidhermit.drag.DragSinkListener;

import android.graphics.Typeface;
import android.view.DragEvent;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnLongClickListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

/**
 * ListView adapter used in conjunction with ConsoleActivity. This initializes the values of
 * each ConsoleEntry and defines event-driven behavior for everything in the ListView
 * (except for the footer).
 */
public class ConsoleEntryAdapter extends ArrayAdapter<ConsoleEntry> {

	private ConsoleActivity mConsole;
	private List<ConsoleEntry> mEntries;
	private Typeface mTypeface;
	private ConsoleEntryHolder mHolder;
	
	public ConsoleEntryAdapter(ConsoleActivity console, List<ConsoleEntry> entries) {
		super(console, R.layout.console_entry, entries);
		mConsole = console;
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
			entryView.setTag(mHolder);
		} else {
			mHolder = (ConsoleEntryHolder) entryView.getTag();
		}
		
		mHolder.num.setText("hermit<" + mEntries.get(position).getNum() + "> ");
		mHolder.num.setTypeface(mTypeface);
		
		PrettyPrinter.setPrettyText(mHolder.contents,
				mEntries.get(position).getContents());
		mHolder.contents.setTypeface(mTypeface);
		
		//A strangely redundant, but necessary, step to get onLongClickListener to work
		final int thepos = position;
		
		entryView.setOnDragListener(new DragSinkListener() {	
			@Override
			public void onDragEntered(View dragView, View dragSink, DragEvent event) {
				dragSink.setBackground(dragSink.getResources().getDrawable(R.drawable.console_text_border));
			}

			@Override
			public void onDragExited(View dragView, View dragSink) {
				dragSink.setBackgroundColor(dragSink.getResources().getColor(android.R.color.transparent));
			}

			@Override
			public void onDragEnded(View dragView, View dragSink) {
				dragSink.setBackgroundColor(dragSink.getResources().getColor(android.R.color.transparent));
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
					return true;
				}
				return false;
			}
		});
		entryView.setOnTouchListener(new OnTouchListener() {
			@Override
			public boolean onTouch(View v, MotionEvent event) {
				if (event.getAction() == MotionEvent.ACTION_DOWN) {
					v.setBackgroundResource(R.drawable.console_text_border);
				} else if (event.getAction() == MotionEvent.ACTION_UP
						|| event.getAction() == MotionEvent.ACTION_OUTSIDE
						|| event.getAction() == MotionEvent.ACTION_CANCEL) {
					v.setBackgroundResource(android.R.color.transparent);
				}
				return false;
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
	}

}
