package edu.kufpg.armatus.console;

import java.util.List;
import java.util.SortedSet;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleWordSearcher.MatchParams;

import android.graphics.Color;
import android.os.Parcel;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.BackgroundColorSpan;
import android.text.style.CharacterStyle;
import android.text.style.ForegroundColorSpan;
import android.view.LayoutInflater;
import android.view.View;
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
	/** Reference to the current console. */
	private ConsoleActivity mConsole;

	/** Reference to the console searcher. */
	private ConsoleWordSearcher mSearcher;

	//	/** A drag listener applied to every {@link ConsoleEntry} {@link View}. */
	//	private OnDragListener mOnDragListener;

	/**
	 * Constructs a new instance with the specified entries.
	 * @param console The {@link ConsoleActivity} to use.
	 * @param entries The {@link ConsoleEntry ConsoleEntries} with which to populate
	 * the {@link ListView}.
	 */
	public ConsoleEntryAdapter(ConsoleActivity console, List<ConsoleEntry> entries) {
		super(console, R.layout.console_entry, entries);
		mConsole = console;
		//		mOnDragListener = new OnDragListener() {
		//			@Override
		//			public boolean onDrag(View v, DragEvent event) {
		//				if (event.getAction() == DragEvent.ACTION_DROP) {
		//					int pos = mConsole.getListView().getPositionForView(v);
		//					List<String> keywords = getItem(pos).getKeywords();
		//					if (!keywords.isEmpty()) {
		//						DragIcon icon = (DragIcon) event.getLocalState();
		//						ConsoleEntryHolder holder = (ConsoleEntryHolder) v.getTag();
		//						holder.draggedOverCommand = icon.getText().toString();
		//						mConsole.openContextMenu(v);
		//					}
		//				}
		//				return true;
		//			}
		//		};
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		ConsoleEntryHolder holder;
		CharSequence entryContents = getItem(position).getFullContents();

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
		holder.contents.setText(entryContents);

		//Highlights search matches
		if (mSearcher != null) {
			String criterion = mSearcher.getCriterion();
			if (mSearcher.isSearching()) {
				if (mSearcher.hasMatches(entryContents.toString())) {
					removeHighlight(holder.contents);
					Spannable contents = new SpannableString(holder.contents.getText());
					SortedSet<Integer> offsets = mSearcher.getMatchOffsets(entryContents.toString());
					for (int offset : offsets) {
						MatchParams params = mSearcher.getSelectedMatch();
						if (position == params.listIndex && offset == params.textViewOffset) {
							MatchBackgroundSpan highlighted = new MatchBackgroundSpan(Color.parseColor("#FFC400"));
							MatchForegroundSpan blackText = new MatchForegroundSpan(Color.BLACK);
							setSpans(contents, offset, offset + criterion.length(), highlighted, blackText);
						} else {
							MatchBackgroundSpan unhighlighted = new MatchBackgroundSpan(Color.DKGRAY);
							MatchForegroundSpan whiteText = new MatchForegroundSpan(Color.WHITE);
							setSpans(contents, offset, offset + criterion.length(), unhighlighted, whiteText);
						}
					}
					holder.contents.setText(contents);
				}
			} else {
				removeHighlight(holder.contents);
			}
		}

		//		convertView.setOnDragListener(mOnDragListener);

		return convertView;
	}

	/**
	 * Restores the reference to the console searcher, which can be destroyed after
	 * device standby or rotation.
	 * @param searcher The {@link ConsoleWordSearcher} to reconnect to.
	 */
	void attachSearcher(ConsoleWordSearcher searcher) {
		mSearcher = searcher;
	}

	/**
	 * Strips any spans used to color {@link TextView} words during a console search.
	 * @param tv The {@code TextView} from which to remove all highlighting.
	 */
	private void removeHighlight(TextView tv) {
		Spannable noHighlight = new SpannableString(tv.getText());
		CharacterStyle[] matchBackgrounds = noHighlight.getSpans(0, noHighlight.length(), MatchBackgroundSpan.class);
		for (CharacterStyle span : matchBackgrounds) {
			noHighlight.removeSpan(span);
		}
		CharacterStyle[] matchForegrounds = noHighlight.getSpans(0, noHighlight.length(), MatchForegroundSpan.class);
		for (CharacterStyle span : matchForegrounds) {
			noHighlight.removeSpan(span);
		}
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
	static class ConsoleEntryHolder {
		public TextView contents;
		public String draggedOverCommand;
	}

	private static class MatchBackgroundSpan extends BackgroundColorSpan {
		public MatchBackgroundSpan(int color) {
			super(color);
		}

		public MatchBackgroundSpan(Parcel src) {
			super(src);
		}
	}

	private static class MatchForegroundSpan extends ForegroundColorSpan {
		public MatchForegroundSpan(int color) {
			super(color);
		}

		public MatchForegroundSpan(Parcel src) {
			super(src);
		}
	}

}