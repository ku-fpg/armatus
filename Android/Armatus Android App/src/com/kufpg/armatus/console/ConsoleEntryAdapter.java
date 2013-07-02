package com.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import com.kufpg.armatus.R;
import com.kufpg.armatus.drag.DragIcon;
import com.kufpg.armatus.drag.DragSinkListener;

import android.graphics.Color;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.BackgroundColorSpan;
import android.text.style.CharacterStyle;
import android.view.ActionMode;
import android.view.DragEvent;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.View.OnDragListener;
import android.view.View.OnLongClickListener;
import android.view.View.OnTouchListener;
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
	public static final int HIGHLIGHTED = R.drawable.console_entry_highlighted;
	public static final int SELECTED = android.R.color.holo_blue_light;
	public static final int HIGHLIGHTED_SELECTED = R.drawable.console_entry_highlighted_selected;
	public static final int TRANSPARENT = android.R.color.transparent;

	private ConsoleActivity mConsole;
	private ListView mListView;
	private List<ConsoleEntry> mEntries, mOriginalEntries;
	private ActionMode mActionMode;
	private Filter mFilter;
	private Object mLock = new Object();
	private CharSequence mConstraint;
	private int mFilterMatches = 0;
	private int mCheckedPos = -1;

	private OnClickListener mOnClickListener = new OnClickListener() {
		@Override
		public void onClick(View v) {
			int clickedPos = mListView.getPositionForView(v);
			if (mCheckedPos != clickedPos) {
				selectEntry(clickedPos);
				mCheckedPos = mListView.getCheckedItemPosition();
			} else {
				hideActionBar();
			}
			notifyDataSetChanged();
		}
	};

	private OnLongClickListener mOnLongClickListener = new OnLongClickListener() {
		@Override
		public boolean onLongClick(View v) {
			mConsole.openContextMenu(v);
			return true;
		}
	};

	private OnTouchListener mOnTouchListener = new OnTouchListener() {
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
	};

	private OnDragListener mOnDragListener = new DragSinkListener() {
		@Override
		public void onDragEntered(View dragView, View dragSink, DragEvent event) {
			dragSink.setBackgroundResource(HIGHLIGHTED);
		}

		@Override
		public void onDragExited(View dragView, View dragSink, DragEvent event) {
			dragSink.setBackgroundResource(TRANSPARENT);
		}

		@Override
		public void onDragEnded(View dragView, View dragSink, DragEvent event) {
			dragSink.setBackgroundResource(TRANSPARENT);
		}

		@Override
		public void onDragDropped(View dragView, View dragSink, DragEvent event) {
			int pos = mListView.getPositionForView(dragSink);
			List<String> keywords = getItem(pos).getKeywords();
			if (!keywords.isEmpty()) {
				mConsole.setTempCommand(((DragIcon) dragView).getCommandName());
				mConsole.openContextMenu(dragSink);
			}
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

		if (mListView.getCheckedItemPosition() == position) {
			holder.layout.setBackgroundResource(SELECTED);
		} else {
			holder.layout.setBackgroundResource(TRANSPARENT);
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
		convertView.setOnLongClickListener(mOnLongClickListener);
		convertView.setOnTouchListener(mOnTouchListener);
		convertView.setOnClickListener(mOnClickListener);

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

	/**
	 * Sets an entry's value as checked and highlights it.
	 * @param pos The position of the entry to select.
	 */
	void selectEntry(int pos) {
		mListView.setItemChecked(pos, true);
		showActionBar(pos);
	}

	/**
	 * Removes an entry's checked value and restores the default background color.
	 * Calling hideActionBar() will call this method.
	 * @param pos The position of the entry to deselect.
	 */
	void deselectEntry() {
		if (mCheckedPos != -1) {
			mListView.setItemChecked(mCheckedPos, false);
			mCheckedPos = -1;
		}
	}

	/**
	 * Note: this gets called in selectEntry()
	 * @param checkedPos
	 */
	void showActionBar(int checkedPos) {
		mActionMode = mConsole.startActionMode(new ConsoleEntryCallback(mConsole, this,
				getItem(checkedPos).getNum(), getItem(checkedPos).getContents()));
	}

	void hideActionBar() {
		if (mActionMode != null) {
			mActionMode.finish();
			mActionMode = null;
		}
	}

}
