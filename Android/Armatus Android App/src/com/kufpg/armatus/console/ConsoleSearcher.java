package com.kufpg.armatus.console;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.Locale;
import java.util.Stack;

import com.google.common.collect.Multimap;
import com.google.common.collect.TreeMultimap;

import android.os.Parcel;
import android.os.Parcelable;

public class ConsoleSearcher implements Parcelable {
	private static int NO_MATCH = -1;
	private ConsoleEntryAdapter mAdapter;
	private String mCriterion;
	private Multimap<String, Integer> mSearchOffsetsMap = TreeMultimap.create();
	private Stack<MatchParams> mPreviousMatches = new Stack<MatchParams>();
	private Stack<MatchParams> mNextMatches = new Stack<MatchParams>();
	private MatchParams mSelectedMatch;
	private int mMatchCount = 0;

	public ConsoleSearcher(ConsoleEntryAdapter adapter) {
		attachAdapter(adapter);
	}

	void attachAdapter(ConsoleEntryAdapter adapter) {
		mAdapter = adapter;
		mAdapter.attachSearcher(this);
	}

	public synchronized MatchParams beginSearch(String criterion) {
		mCriterion = criterion.toLowerCase(Locale.US);
		mMatchCount = 0;
		mSearchOffsetsMap.clear();
		mPreviousMatches.clear();
		mNextMatches.clear();
		mSelectedMatch = null;
		if (!mCriterion.isEmpty()) {
			for (int index = 0; index < mAdapter.getCount(); index++) {
				String entryContents = mAdapter.getItem(index).getContents().toLowerCase(Locale.US);
				Collection<Integer> offsets = mSearchOffsetsMap.get(entryContents);
				if (offsets.isEmpty() && !mSearchOffsetsMap.containsEntry(entryContents, NO_MATCH)) {
					offsets = getMatchOffsets(mCriterion, entryContents);
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

	public synchronized MatchParams continueSearch(Direction direction) {
		MatchParams mCurSelection = null;
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
			mCurSelection = mSelectedMatch;
		}
		mAdapter.notifyDataSetChanged();
		return mCurSelection;
	}

	public synchronized void endSearch() {
		mCriterion = null;
		mMatchCount = 0;
		mSearchOffsetsMap.clear();
		mPreviousMatches.clear();
		mNextMatches.clear();
		mSelectedMatch = null;
		mAdapter.notifyDataSetChanged();
	}

	public synchronized String getCriterion() {
		return mCriterion;
	}

	public synchronized int getMatchesCount() {
		return mMatchCount;
	}

	public synchronized Collection<Integer> getMatchOffsets(String contents) {
		if (hasMatches(contents)) {
			return mSearchOffsetsMap.get(contents.toLowerCase(Locale.US));
		} else {
			return null;
		}
	}

	private static List<Integer> getMatchOffsets(String pattern, String target) {
		if (!pattern.isEmpty()) {
			List<Integer> matches = new ArrayList<Integer>();
			for (int i = target.indexOf(pattern); i >= 0; i = target.indexOf(pattern, i+1)) {
				matches.add(i);
			}

			if (matches.isEmpty()) {
				matches.add(NO_MATCH);
			}
			return matches;
		} else {
			return Arrays.asList(NO_MATCH);
		}
	}

	public synchronized MatchParams getSelectedMatch() {
		return mSelectedMatch;
	}

	public synchronized int getSelectedMatchPosition() {
		if (isSearching()) {
			return mPreviousMatches.size() + 1;
		} else {
			return NO_MATCH;
		}
	}

	public synchronized boolean hasMatches(String contents) {
		if (contents == null) {
			return false;
		}
		Collection<Integer> offsets = mSearchOffsetsMap.get(contents.toLowerCase(Locale.US));
		return !offsets.contains(NO_MATCH) && !offsets.isEmpty();
	}

	public synchronized boolean isSearching() {
		return mCriterion != null;
	}

	public static final Parcelable.Creator<ConsoleSearcher> CREATOR
	= new Parcelable.Creator<ConsoleSearcher>() {
		public ConsoleSearcher createFromParcel(Parcel in) {
			return new ConsoleSearcher(in);
		}

		public ConsoleSearcher[] newArray(int size) {
			return new ConsoleSearcher[size];
		}
	};

	@SuppressWarnings("unchecked")
	private ConsoleSearcher(Parcel in) {
		mCriterion = in.readString();
		mSearchOffsetsMap = (Multimap<String, Integer>) in.readSerializable();
		mPreviousMatches = (Stack<MatchParams>) in.readSerializable();
		mNextMatches = (Stack<MatchParams>) in.readSerializable();
		mSelectedMatch = (MatchParams) in.readSerializable();
		mMatchCount = in.readInt();
	}


	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeString(mCriterion);
		dest.writeSerializable((Serializable) mSearchOffsetsMap);
		dest.writeSerializable(mPreviousMatches);
		dest.writeSerializable(mNextMatches);
		dest.writeSerializable(mSelectedMatch);
		dest.writeInt(mMatchCount);
	}

	public static class MatchParams implements Serializable {
		private static final long serialVersionUID = 2976454656462329956L;
		public final int listIndex;
		public final int textViewOffset;

		public MatchParams(int listIndex, int textViewOffset) {
			this.listIndex = listIndex;
			this.textViewOffset = textViewOffset;
		}

		public MatchParams(MatchParams params) {
			listIndex = params.listIndex;
			textViewOffset = params.textViewOffset;
		}
	}

	public enum Direction { NEXT, PREVIOUS };

}
