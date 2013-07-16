package com.kufpg.armatus.console;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import android.text.Editable;
import android.text.TextWatcher;

import com.google.common.collect.ImmutableSortedSet;
import com.kufpg.armatus.util.StringUtils;

public class WordCompleter implements Serializable, TextWatcher {

	private static final long serialVersionUID = 6187148466800719497L;
	private final SortedSet<String> COMMAND_DICTIONARY;
	private transient ConsoleActivity mConsole;
	private String mPrevPartialWord;
	private SortedSet<String> mFilteredDictionary;

	public WordCompleter(ConsoleActivity console) {
		mConsole = console;
		ImmutableSortedSet.Builder<String> dictBuilder = ImmutableSortedSet.naturalOrder();
		COMMAND_DICTIONARY = dictBuilder.addAll(CommandDispatcher.getCommandNames()).build();
		resetFilter("");
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

}
