package com.kufpg.armatus.console;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.Editable;
import android.text.TextWatcher;

import com.google.common.collect.ImmutableSortedSet;
import com.kufpg.armatus.util.StringUtils;

public class WordCompleter implements Parcelable, TextWatcher {
	private ConsoleActivity mConsole;
	private final SortedSet<String> COMMAND_DICTIONARY;
	private SortedSet<String> mFilteredDictionary;
	private String mPrevPartialWord;

	public WordCompleter(ConsoleActivity console) {
		mConsole = console;
		ImmutableSortedSet.Builder<String> dictBuilder = ImmutableSortedSet.naturalOrder();
		COMMAND_DICTIONARY = dictBuilder.addAll(CommandDispatcher.getCommandNames()).build();
		resetFilter("");
	}
	
	public void attachConsole(ConsoleActivity console) {
		mConsole = console;
	}

	public void filterDictionary(String curPartialWord) {
		if (curPartialWord.length() > mPrevPartialWord.length()) {
			if (curPartialWord.startsWith(mPrevPartialWord)) {
				Iterator<String> iterator = mFilteredDictionary.iterator();
				while (iterator.hasNext()) {
					if (!iterator.next().startsWith(curPartialWord)) {
						iterator.remove();
					}
				}
			} else {
				resetFilter(curPartialWord);
			}
		} else if (curPartialWord.length() < mPrevPartialWord.length()) {
			if (mPrevPartialWord.startsWith(curPartialWord)) {
				for (String word : COMMAND_DICTIONARY) {
					if (word.startsWith(curPartialWord)) {
						mFilteredDictionary.add(word);
					}
				}
			} else {
				resetFilter(curPartialWord);
			}
		} else {
			if (!curPartialWord.equals(mPrevPartialWord)) {
				resetFilter(curPartialWord);
			}
		}
		mPrevPartialWord = curPartialWord;
	}

	public String completeWord(String partialWord) {
		if (mFilteredDictionary.size() == 1) {
			return mFilteredDictionary.first() + " ";
		} else if (mFilteredDictionary.size() > 1) {
			mConsole.showEntryDialog(-1, null, ConsoleActivity.WORD_COMPLETION_TAG);
		}
		return null;
	}

	public List<String> getWordSuggestions() {
		List<String> wordList = new ArrayList<String>();
		wordList.addAll(mFilteredDictionary);
		return wordList;
	}

	private void resetFilter(String curPartialWord) {
		mFilteredDictionary = new TreeSet<String>(COMMAND_DICTIONARY);
		mPrevPartialWord = "";
		filterDictionary(curPartialWord);
	}

	@Override
	public void afterTextChanged(Editable s) {}

	@Override
	public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

	@Override
	public void onTextChanged(CharSequence s, int start, int before, int count) {
		String input = s.toString().trim();
		if (input.split(StringUtils.WHITESPACE).length <= 1) {
			filterDictionary(input);
		}
	}

	public static final Parcelable.Creator<WordCompleter> CREATOR
	= new Parcelable.Creator<WordCompleter>() {
		public WordCompleter createFromParcel(Parcel in) {
			return new WordCompleter(in);
		}

		public WordCompleter[] newArray(int size) {
			return new WordCompleter[size];
		}
	};

	@SuppressWarnings("unchecked")
	private WordCompleter(Parcel in) {
		COMMAND_DICTIONARY = (SortedSet<String>) in.readSerializable();
		mFilteredDictionary = (SortedSet<String>) in.readSerializable();
		mPrevPartialWord = in.readString();
	}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeSerializable((Serializable) COMMAND_DICTIONARY);
		dest.writeSerializable((Serializable) mFilteredDictionary);
		dest.writeString(mPrevPartialWord);
	}

}
