package com.kufpg.androidhermit.test;

import java.util.List;

import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.drag.CommandIcon;
import com.kufpg.androidhermit.drag.DragSinkListener;

import android.graphics.Typeface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

public class TestConsoleEntryAdapter extends ArrayAdapter<TestConsoleEntry> {

	private TestActivity mConsole;
	private List<TestConsoleEntry> mEntries;
	private Typeface mTypeface;
	
	public TestConsoleEntryAdapter(TestActivity console, List<TestConsoleEntry> entries) {
		super(console, R.layout.console_entry, entries);
		mConsole = console;
		mEntries = entries;
		mTypeface = Typeface.createFromAsset(mConsole.getResources().getAssets(),
				TestActivity.TYPEFACE);
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		View entryView = convertView;
		ConsoleEntryHolder holder = null;
		if (entryView == null) {
			LayoutInflater inflater = mConsole.getLayoutInflater();
			entryView = inflater.inflate(R.layout.console_entry, parent, false);
			holder = new ConsoleEntryHolder();
			holder.num = (TextView) entryView.findViewById(R.id.console_entry_num);
			holder.contents = (TextView) entryView.findViewById(R.id.console_entry_contents);
			entryView.setTag(holder);
		} else {
			holder = (ConsoleEntryHolder) entryView.getTag();
		}
		
		holder.num.setText("hermit<" + mEntries.get(position).getNum() + "> ");
		holder.num.setTextSize(TestActivity.DEFAULT_FONT_SIZE);
		holder.num.setTypeface(mTypeface);
		
		//mEntryContents.addTextChangedListener(new TestPrettyPrinter(mEntryContents));
		holder.contents.setText(mEntries.get(position).getContents());
		holder.contents.setTextSize(TestActivity.DEFAULT_FONT_SIZE);
		holder.contents.setTypeface(mTypeface);
		
		mConsole.registerForContextMenu(entryView);
		final int thepos = position;
		final View theview = entryView;
		entryView.setOnDragListener(new DragSinkListener() {
			@Override
			public void onDragEntered(View dragView, View dragSink) {
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
					mConsole.setTempCommand(((CommandIcon) dragView).getCommandName());
					mConsole.setTempKeywords(keywords);
					mConsole.openContextMenu(theview);
				}
			}
		});
		
		return entryView;
	}
	
	static class ConsoleEntryHolder {
		TextView num;
		TextView contents;
	}
	
	public TestActivity getConsole() {
		return mConsole;
	}

}
