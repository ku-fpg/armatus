package edu.kufpg.armatus.console;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import edu.kufpg.armatus.util.StringUtils;

/**
 * Contains values that describe an entry in {@link ConsoleListView}. This class
 * is primarily used for populating {@link android.view.View Views} in {@link
 * ConsoleEntryAdapter}.
 */
public class ConsoleEntry implements Serializable {
	private static final long serialVersionUID = -1808578272659814103L;
	
	/** 
	 * The unique entry number used to identify this entry. Entry numbers begin
	 * at 0 (although the first entry in the console may not be 0 since the number
	 * of entries could exceed the {@link ConsoleActivity#CONSOLE_ENTRY_LIMIT
	 * CONSOLE_ENTRY_LIMIT}).
	 */
	private int mNum;
	
	/** The contents of the entry, not including the {@code hermit<num> }prefix. */
	private String mShortContents = "";
	
	/**
	 * All {@link CommandDispatcher.Keyword Keywords} (as specified in {@link
	 * CommandDispatcher}) that this entry contains.
	 */
	private final List<String> mKeywords = new ArrayList<String>();

	/**
	 * Constructs a new instance with the specified entry number and contents.
	 * @param entryNum the number that will identify this entry.
	 * @param contents what this entry will contain.
	 */
	public ConsoleEntry(int entryNum, String contents) {
		mNum = entryNum;
		if (contents != null) {
			mShortContents = StringUtils.withCharWrap(contents);
			String[] inputArr = contents.split(StringUtils.WHITESPACE);
			for (String word : inputArr) {
				if (CommandDispatcher.isKeyword(word)) {
					mKeywords.add(word);
				}
			}
		}
	}

	/**
	 * Constructs a new instance with the specified {@link ConsoleEntry}'s number
	 * and contents.
	 * @param entry the {@code ConsoleEntry} to copy.
	 */
	public ConsoleEntry(ConsoleEntry entry) {
		this(entry.getNum(), entry.getShortContents());
	}

	/**
	 * Returns the entry's unique number.
	 * @return the number used to identify this entry.
	 */
	public int getNum() {
		return mNum;
	}

	/**
	 * Returns this entry's contents without the {@code hermit<num> }prefix.
	 * @return the unadorned entry contents.
	 */
	public String getShortContents() {
		return mShortContents;
	}
	
	/**
	 * Returns this entry's contents including the {@code hermit<num> }prefix.
	 * @return the entry contents, including the prefix.
	 */
	public String getFullContents() {
		return "hermit<" + getNum() + ">" + StringUtils.NBSP + getShortContents();
	}

	/**
	 * Returns the list of {@link CommandDispatcher.Keyword Keywords} contained in this
	 * entry.
	 * @return the list of {@code Keywords}.
	 */
	public final List<String> getKeywords() {
		return mKeywords;
	}
	
	/**
	 * Edits the current contents by placing the specified string as a new line under
	 * the previous contents.
	 * @param newContents The string to append as a newline.
	 */
	public void appendContents(String newContents) {
		mShortContents += "\n" + newContents;
	}

}