package edu.kufpg.armatus.console;

import java.util.Collection;
import java.util.Locale;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeSet;

import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;

import edu.kufpg.armatus.util.ParcelUtils;

import android.os.Parcel;
import android.os.Parcelable;

/**
 * Searches a console's entries for words that match a given criterion. Because this
 * class can potentially store a lot of data, it is recommended that if you need to
 * destroy a {@code ConsoleSearcher}, you should parcel it and reload it later
 * instead of recreating a new instance every time.
 */
public class ConsoleWordSearcher implements Parcelable {
	/** Value indicating that no matching indexes were found. */
	private static final int NO_MATCH = -1;
	
	/** Reference to the console's adapter (used for indicating when the adapter should
	 * update the selection highlight). */
	private ConsoleEntryAdapter mAdapter;
	
	/** The current search criterion. */
	private String mCriterion;
	
	/** Maps entry contents (in all lowercase) to their matching search offsets. */
	private SortedSetMultimap<String, Integer> mSearchOffsetsMap = TreeMultimap.create();
	
	/** Stacks the previously highlighted matches (from bottom to top in ascending
	 * console order). */
	private Stack<MatchParams> mPreviousMatches = new Stack<MatchParams>();
	
	/** Stacks the matches to be highlighted next after {@link #mSelectedMatch} (from
	 * top to bottom in ascending console order). */
	private Stack<MatchParams> mNextMatches = new Stack<MatchParams>();
	
	/** The currently selected highlighted match. */
	private MatchParams mSelectedMatch;
	
	/** Tracks the number of matches for {@link #mCriterion}. */
	private int mMatchCount = 0;

	/**
	 * Constructs a new instance with a reference to the specified adapter.
	 * @param adapter The {@link ConsoleEntryAdapter} to reference.
	 */
	public ConsoleWordSearcher(ConsoleEntryAdapter adapter) {
		attachAdapter(adapter);
	}

	/**
	 * Restores the reference to the console's adapter, which can be destroyed after
	 * device standby or rotation.
	 * @param adapter The {@link ConsoleEntryAdapter} to reconnect to.
	 */
	void attachAdapter(ConsoleEntryAdapter adapter) {
		mAdapter = adapter;
		mAdapter.attachSearcher(this);
	}

	/**
	 * Begin a new search with a specified search criterion.
	 * @param criterion The string to search for.
	 * @return The {@link MatchParams} of the first match, or {@code null} if there
	 * are no matches.
	 */
	public synchronized MatchParams beginSearch(String criterion) {
		mCriterion = criterion.toLowerCase(Locale.US);
		mMatchCount = 0;
		mSearchOffsetsMap.clear();
		mPreviousMatches.clear();
		mNextMatches.clear();
		mSelectedMatch = null;
		if (!mCriterion.isEmpty()) {
			for (int index = 0; index < mAdapter.getCount(); index++) {
				String entryContents = mAdapter.getItem(index).getFullContents().toString().toLowerCase(Locale.US);
				SortedSet<Integer> offsets = mSearchOffsetsMap.get(entryContents);
				if (offsets.isEmpty() && !mSearchOffsetsMap.containsEntry(entryContents, NO_MATCH)) {
					offsets = getMatchIndexes(mCriterion, entryContents);
				}
				for (int offset : offsets) {
					if (offset != NO_MATCH) {
						//Add them to beginning of stack to avoid having to reverse order later
						mNextMatches.add(0, new MatchParams(index, offset));
						mMatchCount++;
						mSearchOffsetsMap.put(entryContents, offset);
					}
				}
			}
			if (mMatchCount > 0) {
				mSelectedMatch = mNextMatches.pop();
			}
		}
		mAdapter.notifyDataSetChanged();
		return mSelectedMatch;
	}

	/**
	 * Resume the ongoing search in the specified {@link SearchDirection}.
	 * @param direction Either {@link SearchDirection#NEXT} or {@link SearchDirection#PREVIOUS}.
	 * @return The {@link MatchParams} of the newly selected match, or {@code null}
	 * if there are no matches.
	 */
	public synchronized MatchParams continueSearch(SearchDirection direction) {
		MatchParams curSelection = null;
		if (mMatchCount > 0) {
			Stack<MatchParams> popper = null, pusher = null;
			switch (direction) {
			case NEXT:
				popper = mNextMatches;
				pusher = mPreviousMatches;
				break;
			case PREVIOUS:
				popper = mPreviousMatches;
				pusher = mNextMatches;
			}

			if (!popper.empty()) {
				pusher.push(mSelectedMatch);
				mSelectedMatch = popper.pop();
			} else {
				while (!pusher.empty()) {
					popper.push(mSelectedMatch);
					mSelectedMatch = pusher.pop();
				}
			}		
			curSelection = mSelectedMatch;
		}
		mAdapter.notifyDataSetChanged();
		return curSelection;
	}

	/**
	 * Ends the current search, removing any highlighting.
	 */
	public synchronized void endSearch() {
		mCriterion = null;
		mMatchCount = 0;
		mSearchOffsetsMap.clear();
		mPreviousMatches.clear();
		mNextMatches.clear();
		mSelectedMatch = null;
		mAdapter.notifyDataSetChanged();
	}

	/**
	 * Returns the current search criterion.
	 * @return the current search criterion or {@code null} if there is no
	 * ongoing search.
	 */
	public synchronized String getCriterion() {
		return mCriterion;
	}

	/**
	 * Returns the number of matches for the current search criterion.
	 * @return the number of current matches. If there is no ongoing search, 0 is
	 * returned.
	 */
	public synchronized int getMatchesCount() {
		return mMatchCount;
	}

	/**
	 * Returns a sorted set of {@link android.widget.TextView TextView} offsets for the
	 * specified string's matches to the current search criterion.
	 * @param contents The string whose matches should be returned. The string must match
	 * the contents of a current {@link ConsoleEntry} (ignoring case) or this method will
	 * not return the correct result.
	 * @return a {@link SortedSet} of the string matches' {@code TextView} offsets.
	 * If there are no matches, {@code null} is returned.
	 */
	public synchronized SortedSet<Integer> getMatchOffsets(String contents) {
		if (hasMatches(contents)) {
			return mSearchOffsetsMap.get(contents.toLowerCase(Locale.US));
		} else {
			return null;
		}
	}

	/**
	 * Returns a sorted set of string starting indexes (inclusive) where {@code pattern}
	 * is located in {@code target}.
	 * @param pattern The string to search for in {@code target}.
	 * @param target The string in which {@code pattern} matches are searched for.
	 * @return A {@link SortedSet} of indexes indicating matches. If there are no
	 * matches, the set will only contain {@link #NO_MATCH}.
	 */
	private static SortedSet<Integer> getMatchIndexes(String pattern, String target) {
		SortedSet<Integer> matches = new TreeSet<Integer>();
		if (!pattern.isEmpty()) {
			for (int i = target.indexOf(pattern); i >= 0; i = target.indexOf(pattern, i+1)) {
				matches.add(i);
			}

			if (matches.isEmpty()) {
				matches.add(NO_MATCH);
			}
		} else {
			matches.add(NO_MATCH);
		}
		return matches;
	}

	/**
	 * Return the parameters of the selected match.
	 * @return the currently selected {@link MatchParams} or {@code null} if there
	 * is either no ongoing search or no search matches.
	 */
	public synchronized MatchParams getSelectedMatch() {
		return mSelectedMatch;
	}

	/**
	 * Returns the position of the selected match in relation to all other matches.
	 * The first position is 1.
	 * @return the position of the currently selected match or {@link #NO_MATCH} if
	 * there is either no ongoing search or no search matches.
	 */
	public synchronized int getSelectedMatchPosition() {
		if (isSearching() && mMatchCount > 0) {
			return mPreviousMatches.size() + 1;
		} else {
			return NO_MATCH;
		}
	}

	/**
	 * Determines if the specified string contains any matches with the current search
	 * criterion.
	 * @param contents The string to check for matches. The string must match the
	 * contents of a current {@link ConsoleEntry} (ignoring case) or this method will not
	 * return the correct result.
	 * @return if the string contains at least one match with the current search criterion.
	 */
	public synchronized boolean hasMatches(String contents) {
		if (contents == null) {
			return false;
		}
		Collection<Integer> offsets = mSearchOffsetsMap.get(contents.toLowerCase(Locale.US));
		return !offsets.contains(NO_MATCH) && !offsets.isEmpty();
	}

	/**
	 * Returns if this {@link ConsoleWordSearcher} is currently searching.
	 * @return if a search is ongoing.
	 */
	public synchronized boolean isSearching() {
		return mCriterion != null;
	}

	public static final Parcelable.Creator<ConsoleWordSearcher> CREATOR
	= new Parcelable.Creator<ConsoleWordSearcher>() {
		public ConsoleWordSearcher createFromParcel(Parcel in) {
			return new ConsoleWordSearcher(in);
		}

		public ConsoleWordSearcher[] newArray(int size) {
			return new ConsoleWordSearcher[size];
		}
	};

	private ConsoleWordSearcher(Parcel in) {
		mCriterion = in.readString();
		ParcelUtils.readMultimap(mSearchOffsetsMap, in, ConsoleWordSearcher.class.getClassLoader());
		in.readTypedList(mPreviousMatches, MatchParams.CREATOR);
		in.readTypedList(mNextMatches, MatchParams.CREATOR);
		mSelectedMatch = in.readParcelable(ConsoleWordSearcher.class.getClassLoader());
		mMatchCount = in.readInt();
	}


	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeString(mCriterion);
		ParcelUtils.writeMultimap(mSearchOffsetsMap, dest);
		dest.writeTypedList(mPreviousMatches);
		dest.writeTypedList(mNextMatches);
		dest.writeParcelable(mSelectedMatch, flags);
		dest.writeInt(mMatchCount);
	}

	/** The parameters of a search match index. */
	public static class MatchParams implements Parcelable {
		/** The index in the {@link android.widget.ListView ListView} where the
		 * match occurs. */
		public final int listIndex;
		
		/** The offset in the {@link android.widget.TextView TextView} where the
		 * match occurs. */
		public final int textViewOffset;

		/**
		 * Constructs a new instance with the specified {@link android.widget.ListView
		 * ListView} index and {@link android.widget.TextView TextView} offset.
		 * @param listIndex The list index.
		 * @param textViewOffset The {@code TextView} offset.
		 */
		public MatchParams(int listIndex, int textViewOffset) {
			this.listIndex = listIndex;
			this.textViewOffset = textViewOffset;
		}

		/**
		 * Constructs a new instance from the specified {@link MatchParams}.
		 * @param params The {@code MatchParams} to copy.
		 */
		public MatchParams(MatchParams params) {
			listIndex = params.listIndex;
			textViewOffset = params.textViewOffset;
		}
		
		public static Parcelable.Creator<MatchParams> CREATOR =
				new Parcelable.Creator<MatchParams>() {
			@Override
			public MatchParams createFromParcel(Parcel source) {
				int listIndex = source.readInt();
				int textViewOffset = source.readInt();
				return new MatchParams(listIndex, textViewOffset);
			}

			@Override
			public MatchParams[] newArray(int size) {
				return new MatchParams[size];
			}
		};

		@Override
		public int describeContents() {
			return 0;
		}

		@Override
		public void writeToParcel(Parcel dest, int flags) {
			dest.writeInt(listIndex);
			dest.writeInt(textViewOffset);
		}
	}

	/** The possible ways to look up the next match. */
	public enum SearchDirection {
		/** Search for the next-highest {@link android.widget.TextView TextView} offset.
		 * If there is none, search for the first offset in the next-highest {@link
		 * android.widget.ListView ListView} index. If there is none, wrap around to the
		 * very first match.  */
		NEXT,
		
		/** Search for the next-lowest {@link android.widget.TextView TextView} offset.
		 * If there is none, search for the last offset in the next-lowest {@link
		 * android.widget.ListView ListView} index. If there is none, wrap around to the
		 * very last match.  */
		PREVIOUS };

}
