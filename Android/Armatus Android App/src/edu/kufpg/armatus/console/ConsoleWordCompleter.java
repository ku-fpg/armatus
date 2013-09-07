package edu.kufpg.armatus.console;

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

import edu.kufpg.armatus.util.StringUtils;

/**
 * A {@link TextWatcher} that analyzes user input and can make suggestions to complete
 * the input based on a dictionary. Because the dictionary can potentially be very large,
 * it is recommended that if you need to destroy a {@code WordCompleter}, you should
 * parcel it and reload it later instead of recreating a new instance every time.
 */
public class ConsoleWordCompleter implements Parcelable, TextWatcher {
	
	/** A subset of {@link #COMMAND_DICTIONARY} containing only those commands which
	 * match the word to be completed. */
	private SortedSet<String> mFilteredDictionary;
	
	/**
	 * The previous word that was used to filter the dictionary. Comparing this to the
	 * most recent word can be useful for optimizing the filter process.
	 */
	private String mPrevPartialWord;
	
	/**
	 * Reference to the current console.
	 */
	private ConsoleActivity mConsole;

	/**
	 * Constructs a new instance and populates the dictionary using the commands listed
	 * in {@link CustomCommandDispatcher}.
	 * @param console reference to the current console.
	 */
	public ConsoleWordCompleter(ConsoleActivity console, SortedSet<String> commandDictionary) {
		mConsole = console;
		resetFilter("");
	}
	
	/**
	 * Restores this {@link ConsoleWordCompleter}'s reference to the current console, which
	 * can be destroyed after device standby or rotation.
	 * @param console The {@link ConsoleActivity} to reconnect to.
	 */
	public void attachConsole(ConsoleActivity console) {
		mConsole = console;
	}

	/**
	 * Alters the suggestions for completing the current input.
	 * @param curPartialWord The current input.
	 */
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
				for (String word : Commands.getCommands()) {
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

	/**
	 * Returns a suggestion to complete the current input. If there is more than one
	 * suggestion, this will return {@code null} and instead open a dialog listing
	 * all of the suggestions.
	 * @param partialWord The current input.
	 * @return The suggestion for completing the current input, or {@code null} if
	 * there is more than one suggestion.
	 */
	public String completeWord(String partialWord) {
		if (mFilteredDictionary.size() == 1) {
			return mFilteredDictionary.first() + StringUtils.NBSP;
		} else if (mFilteredDictionary.size() > 1) {
			mConsole.showWordCompletionDialog(getWordSuggestions());
		}
		return null;
	}

	/**
	 * Returns all of the current suggestions for completing the current input.
	 * @return a list of suggested words.
	 */
	public List<String> getWordSuggestions() {
		List<String> wordList = new ArrayList<String>();
		wordList.addAll(mFilteredDictionary);
		return wordList;
	}

	/**
	 * Rebuilds {@link #mFilteredDictionary}.
	 * @param curPartialWord The new word to considered for completion when
	 * constructing {@code mFilteredDictionary}.
	 */
	private void resetFilter(String curPartialWord) {
		mFilteredDictionary = new TreeSet<String>(Commands.getCommands());
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

	public static final Parcelable.Creator<ConsoleWordCompleter> CREATOR
	= new Parcelable.Creator<ConsoleWordCompleter>() {
		public ConsoleWordCompleter createFromParcel(Parcel in) {
			return new ConsoleWordCompleter(in);
		}

		public ConsoleWordCompleter[] newArray(int size) {
			return new ConsoleWordCompleter[size];
		}
	};

	@SuppressWarnings("unchecked")
	private ConsoleWordCompleter(Parcel in) {
		mFilteredDictionary = (SortedSet<String>) in.readSerializable();
		mPrevPartialWord = in.readString();
	}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeSerializable((Serializable) mFilteredDictionary);
		dest.writeString(mPrevPartialWord);
	}

}
