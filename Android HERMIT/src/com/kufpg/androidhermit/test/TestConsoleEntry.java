package com.kufpg.androidhermit.test;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.kufpg.androidhermit.console.CommandDispatcher;

/**
 * Class containing values that are binded to Views in TestConsoleEntryAdapter.
 */
public class TestConsoleEntry implements Serializable {

	private static final long serialVersionUID = -1808578272659814103L;
	private int mNum;
	private String mContents = "";
	private final ArrayList<String> mKeywords = new ArrayList<String>();

	public TestConsoleEntry(String contents, int entryNum) {
		mNum = entryNum;
		if (contents != null) {
			mContents = contents;
			String[] inputArr = contents.split(TestActivity.WHITESPACE);
			for (String word : inputArr) {
				if (CommandDispatcher.isKeyword(word)) {
					mKeywords.add(word);
				}
			}
		}
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

}