package com.kufpg.armatus.console;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;


/**
 * Class containing values that are binded to Views in ConsoleEntryAdapter.
 */
public class ConsoleEntry implements Serializable {

	private static final long serialVersionUID = -1808578272659814103L;
	private int mNum;
	private String mContents = "";
	private final ArrayList<String> mKeywords = new ArrayList<String>();
	private boolean mIsWaiting = false;

	public ConsoleEntry(int entryNum, String contents) {
		mNum = entryNum;
		if (contents != null) {
			mContents = contents;
			String[] inputArr = contents.split(ConsoleActivity.WHITESPACE);
			for (String word : inputArr) {
				if (CommandDispatcher.isKeyword(word)) {
					mKeywords.add(word);
				}
			}
		}
	}
	
	public ConsoleEntry(int entryNum, String contents, boolean isWaiting) {
		this(entryNum, contents);
		mIsWaiting = isWaiting;
	}
	
	public int getNum() {
		return mNum;
	}

	public String getContents() {
		return mContents;
	}

	public final List<String> getKeywords() {
		return mKeywords;
	}
	
	public boolean isWaiting() {
		return mIsWaiting;
	}
	
	public void appendContents(String newContents) {
		mContents += "\n" + newContents;
	}

}