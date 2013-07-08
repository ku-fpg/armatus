package com.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import com.kufpg.armatus.R;
import com.kufpg.armatus.drag.DragIcon;

import android.graphics.Color;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.BackgroundColorSpan;
import android.text.style.CharacterStyle;
import android.view.DragEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnDragListener;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Filter;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

/**
 * ListView adapter used in conjunction with ConsoleActivity. This initializes the values of
 * each ConsoleEntry and defines event-driven behavior for everything in the ListView
 * (except for the footer).
 */
public class ConsoleEntryAdapter extends ArrayAdapter<ConsoleEntry> {
	private ConsoleActivity mConsole;
	private ListView mListView;
	private List<ConsoleEntry> mEntries, mOriginalEntries;
	private Filter mFilter;
	private Object mLock = new Object();
	private CharSequence mConstraint;
	private int mFilterMatches = 0;
	
	private OnDragListener mOnDragListener = new OnDragListener() {
		@Override
		public boolean onDrag(View v, DragEvent event) {
			if (event.getAction() == DragEvent.ACTION_DROP) {
				int pos = mListView.getPositionForView(v);
				List<String> keywords = getItem(pos).getKeywords();
				if (!keywords.isEmpty()) {
					DragIcon icon = (DragIcon) event.getLocalState();
					mConsole.setTempCommand(icon.getCommandName());
					mConsole.openContextMenu(v);
				}
			}
			return true;
		}
	};

	public ConsoleEntryAdapter(ConsoleActivity console, List<ConsoleEntry> entries) {
		super(console, R.layout.console_entry, entries);
		mConsole = console;
		mListView = mConsole.getListView();
		mEntries = entries;
	}

	public ConsoleEntryAdapter(ConsoleActivity console, List<ConsoleEntry> entries, List<ConsoleEntry> originals) {
		this(console, entries);
		mOriginalEntries = originals;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		ConsoleEntryHolder holder;
		if (convertView == null) { //If this is a new ConsoleEntry
			LayoutInflater inflater = mConsole.getLayoutInflater();
			convertView = inflater.inflate(R.layout.console_entry, parent, false);
			holder = new ConsoleEntryHolder();
			holder.layout = (RelativeLayout) convertView.findViewById(R.id.console_entry_layout);
			holder.num = (TextView) convertView.findViewById(R.id.console_entry_num);
			holder.contents = (TextView) convertView.findViewById(R.id.console_entry_contents);
			holder.loader = (RelativeLayout) convertView.findViewById(R.id.console_loading);
			convertView.setTag(holder);
		} else {
			holder = (ConsoleEntryHolder) convertView.getTag();
		}

		holder.num.setText("hermit<" + getItem(position).getNum() + "> ");
		holder.num.setTypeface(ConsoleActivity.TYPEFACE);
		holder.contents.setTypeface(ConsoleActivity.TYPEFACE);
		PrettyPrinter.setPrettyText(holder.contents, getItem(position).getContents());

		if (mConstraint != null) {
			String constraint = mConstraint.toString();
			if (!constraint.equalsIgnoreCase(holder.prevConstraint)) {
				removeHighlight(holder.contents);
				if (!constraint.isEmpty()) {
					CharSequence contentText = holder.contents.getText();
					List<Integer> matches = getMatchIndexes(constraint,	contentText.toString());
					if (matches != null) {
						Spannable contents = new SpannableString(contentText);
						for (int index : matches) {
							CharacterStyle highlight = new BackgroundColorSpan(Color.DKGRAY);
							contents.setSpan(highlight, index, index + constraint.length(), 0);
						}
						holder.contents.setText(contents);
					}
				}
				holder.prevConstraint = constraint;
			}
		} else {
			holder.prevConstraint = null;
			removeHighlight(holder.contents);
		}

		if (!getItem(position).isWaiting()) {
			holder.loader.setVisibility(View.GONE);
		} else {
			holder.loader.setVisibility(View.VISIBLE);
		}

		convertView.setOnDragListener(mOnDragListener);

		return convertView;
	}

	@Override
	public Filter getFilter() {
		if (mFilter == null) {
			mFilter = new Filter() {
				@Override
				protected FilterResults performFiltering(CharSequence constraint) {
					mConsole.setInputEnabled(false);
					mConstraint = constraint;
					FilterResults results = new FilterResults();

					if (mOriginalEntries == null) {
						synchronized (mLock) {
							mOriginalEntries = new ArrayList<ConsoleEntry>(mEntries);
						}
					}

					if (constraint == null) {
						List<ConsoleEntry> list;
						synchronized (mLock) {
							list = new ArrayList<ConsoleEntry>(mOriginalEntries);
						}
						results.values = list;
						results.count = list.size();
						mOriginalEntries = null;
						mConsole.setInputEnabled(true);
					} else {
						String prefixString = constraint.toString().toLowerCase(Locale.US);
						List<ConsoleEntry> entries;
						synchronized (mLock) {
							entries = new ArrayList<ConsoleEntry>(mOriginalEntries);
						}

						final List<ConsoleEntry> newEntries = new ArrayList<ConsoleEntry>();
						for (final ConsoleEntry entry : entries) {
							final String contents = entry.getContents().toLowerCase(Locale.US);
							if (contents.contains(prefixString)) {
								newEntries.add(entry);
							}
						}
						results.values = newEntries;
						mFilterMatches = newEntries.size();
					}

					return results;
				}

				@SuppressWarnings("unchecked")
				@Override
				protected void publishResults(CharSequence constraint, FilterResults results) {
					mEntries.clear();
					for (ConsoleEntry entry : (List<ConsoleEntry>) results.values) {
						mEntries.add(entry);
					}
					notifyDataSetChanged();
				}
			};
		}
		return mFilter;
	}

	/**
	 * Helper class that stores the views displaying the data of a ConsoleEntry.
	 * This is supposed to improve performance, if StackOverflow is to be believed.
	 */
	static class ConsoleEntryHolder {
		public RelativeLayout layout;
		public TextView num;
		public TextView contents;
		public RelativeLayout loader;
		public String prevConstraint;
	}

	public int getFilterMatches() {
		return mFilterMatches;
	}

	public boolean isFiltering() {
		return mConstraint != null;
	}

	private static List<Integer> getMatchIndexes(String match, String word) {
		String m = match.toLowerCase(Locale.US);
		String w = word.toLowerCase(Locale.US);
		List<Integer> matches = new ArrayList<Integer>();
		for (int i = w.indexOf(m); i >= 0; i = w.indexOf(m, i+1)) {
			matches.add(i);
		}
		if (!matches.isEmpty()) {
			return matches;
		} else {
			return null;
		}
	}

	private void removeHighlight(TextView tv) {
		Spannable noHighlight = new SpannableString(tv.getText());
		CharacterStyle[] spans = noHighlight.getSpans(0, noHighlight.length(), BackgroundColorSpan.class);
		if (spans.length > 0) {
			for (CharacterStyle span : spans) {
				noHighlight.removeSpan(span);
			}
			tv.setText(noHighlight);
		}
	}

}
