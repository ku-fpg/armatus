package com.kufpg.armatus.console;

import java.util.Collection;
import java.util.List;

import com.kufpg.armatus.R;
import com.kufpg.armatus.console.ConsoleSearcher.MatchParams;
import com.kufpg.armatus.drag.DragIcon;

import android.graphics.Color;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.BackgroundColorSpan;
import android.text.style.CharacterStyle;
import android.text.style.ForegroundColorSpan;
import android.view.DragEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnDragListener;
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
	private static final CharacterStyle BLACK_TEXT = new ForegroundColorSpan(Color.BLACK);
	private ConsoleActivity mConsole;
	private ListView mListView;
	private ConsoleSearcher mSearcher;

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
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		ConsoleEntryHolder holder;
		String entryContents = getItem(position).getContents();

		if (convertView == null) { //If this is a new ConsoleEntry
			LayoutInflater inflater = mConsole.getLayoutInflater();
			convertView = inflater.inflate(R.layout.console_entry, parent, false);
			holder = new ConsoleEntryHolder();
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
		PrettyPrinter.setPrettyText(holder.contents, entryContents);

		if (mSearcher != null) {
			String criterion = mSearcher.getCriterion();
			if (mSearcher.isSearching()) {
				if (mSearcher.hasMatches(entryContents)) {
					removeHighlight(holder.contents);
					Spannable contents = new SpannableString(holder.contents.getText());
					Collection<Integer> offsets = mSearcher.getMatchOffsets(entryContents);
					for (int offset : offsets) {
						CharacterStyle highlight;
						MatchParams params = mSearcher.getSelectedMatch();
						if (position == params.getListIndex() && offset == params.getTextViewOffset()) {
							highlight = new BackgroundColorSpan(Color.YELLOW);
							setSpans(contents, offset, offset + criterion.length(), highlight, BLACK_TEXT);
						} else {
							highlight = new BackgroundColorSpan(Color.DKGRAY);
							contents.setSpan(highlight, offset, offset + criterion.length(), 0);
						}
					}
					holder.contents.setText(contents);
				}
			} else {
				removeHighlight(holder.contents);
			}
		}

		if (!getItem(position).isWaiting()) {
			holder.loader.setVisibility(View.GONE);
		} else {
			holder.loader.setVisibility(View.VISIBLE);
		}

		convertView.setOnDragListener(mOnDragListener);

		return convertView;
	}

	void attachSearcher(ConsoleSearcher searcher) {
		mSearcher = searcher;
	}

	private void removeHighlight(TextView tv) {
		Spannable noHighlight = new SpannableString(tv.getText());
		CharacterStyle[] backgroundSpans = noHighlight.getSpans(0, noHighlight.length(), BackgroundColorSpan.class);
		for (CharacterStyle span : backgroundSpans) {
			noHighlight.removeSpan(span);
		}
		noHighlight.removeSpan(BLACK_TEXT);
		tv.setText(noHighlight);
	}

	private void setSpans(Spannable spannable, int start, int end, CharacterStyle... spans) {
		for (CharacterStyle span : spans) {
			spannable.setSpan(span, start, end, 0);
		}
	}

	private static class ConsoleEntryHolder {
		public TextView num;
		public TextView contents;
		public RelativeLayout loader;
	}

}