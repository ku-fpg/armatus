package com.kufpg.armatus.console;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.kufpg.armatus.util.StringUtils;

/**
 * Class containing values that are binded to Views in ConsoleEntryAdapter.
 */
public class ConsoleEntry implements Serializable {
	private static final long serialVersionUID = -1808578272659814103L;
	private int mNum;
	private String mShortContents = "";
	private final List<String> mKeywords = new ArrayList<String>();

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

	public ConsoleEntry(ConsoleEntry entry) {
		this(entry.getNum(), entry.getShortContents());
	}

	public int getNum() {
		return mNum;
	}

	public String getShortContents() {
		return mShortContents;
	}
	
	public String getFullContents() {
		return "hermit<" + getNum() + ">" + StringUtils.NBSP + getShortContents();
	}

	public final List<String> getKeywords() {
		return mKeywords;
	}
	
	public void appendContents(String newContents) {
		mShortContents += "\n" + newContents;
	}

}