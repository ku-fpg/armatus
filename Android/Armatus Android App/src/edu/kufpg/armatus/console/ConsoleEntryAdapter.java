package edu.kufpg.armatus.console;

import java.util.List;
import java.util.SortedSet;

import android.content.Context;
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
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleWordSearcher.MatchParams;

public class ConsoleEntryAdapter extends BaseExpandableListAdapter {
	private final LayoutInflater mInflater;
	private final List<ConsoleEntry> mEntryList;
	private ConsoleWordSearcher mSearcher;

	public ConsoleEntryAdapter(Context context, List<ConsoleEntry> entryList) {
		mInflater = LayoutInflater.from(context);
		mEntryList = entryList;
	}

	@Override
	public int getGroupCount() {
		return mEntryList.size();
	}

	@Override
	public int getChildrenCount(int groupPosition) {
		return getGroup(groupPosition).getContentLines().size();
	}

	@Override
	public ConsoleEntry getGroup(int groupPosition) {
		return mEntryList.get(groupPosition);
	}

	@Override
	public CharSequence getChild(int groupPosition, int childPosition) {
		return getGroup(groupPosition).getContentLines().get(childPosition);
	}

	@Override
	public long getGroupId(int groupPosition) {
		return groupPosition;
	}

	@Override
	public long getChildId(int groupPosition, int childPosition) {
		return childPosition;
	}

	@Override
	public boolean hasStableIds() {
		return true;
	}

	@Override
	public View getGroupView(int groupPosition, boolean isExpanded,
			View convertView, ViewGroup parent) {
		return mInflater.inflate(R.layout.console_entry_group, null);
	}

	@Override
	public View getChildView(int groupPosition, int childPosition,
			boolean isLastChild, View convertView, ViewGroup parent) {
		ConsoleEntryHolder holder;
		if (convertView == null) {
			convertView = mInflater.inflate(R.layout.console_entry_child, null);
			holder = new ConsoleEntryHolder();
			holder.contents = (TextView) convertView.findViewById(R.id.console_entry_contents);
			convertView.setTag(holder);
		} else {
			holder = (ConsoleEntryHolder) convertView.getTag();
		}

		CharSequence entryContents = getChild(groupPosition, childPosition);
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
						if (groupPosition == params.entryIndex
								&& childPosition == params.lineIndex
								&& offset == params.textViewOffset) {
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

		return convertView;
	}

	@Override
	public boolean isChildSelectable(int groupPosition, int childPosition) {
		return true;
	}

	/**
	 * Restores the reference to the console searcher, which can be destroyed after
	 * device standby or rotation.
	 * @param searcher The {@link ConsoleWordSearcher} to reconnect to.
	 */
	void attachSearcher(ConsoleWordSearcher searcher) {
		mSearcher = searcher;
	}

	/** Holds {@link TextView} reference for efficiency purposes. */
	static class ConsoleEntryHolder {
		public TextView contents;
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
