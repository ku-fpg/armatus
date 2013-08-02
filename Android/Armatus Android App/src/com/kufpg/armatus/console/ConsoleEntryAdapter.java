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
import android.widget.TextView;

/**
 * {@link android.widget.ListAdapter ListAdapter} used to populate a {@link ConsoleListView}.
 * This styles each {@link ConsoleEntry} and defines their drag event behavior when {@link
 * DragIcons} are being dragged (except for the footer, which does not react).
 */
public class ConsoleEntryAdapter extends ArrayAdapter<ConsoleEntry> {
	/** Text color used alongside a yellow highlight (to improve readability). */
	private static final CharacterStyle BLACK_TEXT = new ForegroundColorSpan(Color.BLACK);
	
	/** Reference to the current console. */
	private ConsoleActivity mConsole;

	/** Reference to the console searcher. */
	private ConsoleSearcher mSearcher;
	
	/** A drag listener applied to every {@link ConsoleEntry} {@link View}. */
	private OnDragListener mOnDragListener;

	/**
	 * Constructs a new instance with the specified entries.
	 * @param console The {@link ConsoleActivity} to use.
	 * @param entries The {@link ConsoleEntry ConsoleEntries} with which to populate
	 * the {@link ListView}.
	 */
	public ConsoleEntryAdapter(ConsoleActivity console, List<ConsoleEntry> entries) {
		super(console, R.layout.console_entry, entries);
		mConsole = console;
		mOnDragListener = new OnDragListener() {
			@Override
			public boolean onDrag(View v, DragEvent event) {
				if (event.getAction() == DragEvent.ACTION_DROP) {
					int pos = mConsole.getListView().getPositionForView(v);
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
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		ConsoleEntryHolder holder;
		String entryContents = getItem(position).getFullContents();

		if (convertView == null) {
			LayoutInflater inflater = mConsole.getLayoutInflater();
			convertView = inflater.inflate(R.layout.console_entry, parent, false);
			holder = new ConsoleEntryHolder();
			holder.contents = (TextView) convertView.findViewById(R.id.console_entry_contents);
			convertView.setTag(holder);
		} else {
			holder = (ConsoleEntryHolder) convertView.getTag();
		}

		holder.contents.setTypeface(ConsoleActivity.TYPEFACE);
		if (!entryContents.equals(holder.contents.toString())) {
			PrettyPrinter.setPrettyText(holder.contents, entryContents);
		}

		//Highlights search matches
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
						if (position == params.listIndex && offset == params.textViewOffset) {
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

		convertView.setOnDragListener(mOnDragListener);

		return convertView;
	}

	/**
	 * Restores the reference to the console searcher, which can be destroyed after
	 * device standby or rotation.
	 * @param searcher The {@link ConsoleSearcher} to reconnect to.
	 */
	void attachSearcher(ConsoleSearcher searcher) {
		mSearcher = searcher;
	}

	/**
	 * Strips any spans used to color {@link TextView} words during a console search.
	 * @param tv The {@code TextView} from which to remove all highlighting.
	 */
	private void removeHighlight(TextView tv) {
		Spannable noHighlight = new SpannableString(tv.getText());
		CharacterStyle[] backgroundSpans = noHighlight.getSpans(0, noHighlight.length(), BackgroundColorSpan.class);
		for (CharacterStyle span : backgroundSpans) {
			noHighlight.removeSpan(span);
		}
		noHighlight.removeSpan(BLACK_TEXT);
		tv.setText(noHighlight);
	}

	/**
	 * Utility method for setting multiple spans in succession.
	 * @param spannable The object on which {@code spans} should be set.
	 * @param start The starting index (inclusive) for {@code spans}.
	 * @param end The ending index (exclusive) for {@code spans}.
	 * @param spans The series of {@link CharacterStyle}s to apply to {@code spannable}.
	 */
	private void setSpans(Spannable spannable, int start, int end, CharacterStyle... spans) {
		for (CharacterStyle span : spans) {
			spannable.setSpan(span, start, end, 0);
		}
	}

	/** Holds {@link TextView} reference for efficiency purposes. */
	private static class ConsoleEntryHolder {
		public TextView contents;
	}

}