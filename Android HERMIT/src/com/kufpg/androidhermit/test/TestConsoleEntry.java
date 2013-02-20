package com.kufpg.androidhermit.test;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.kufpg.androidhermit.console.CommandDispatcher;

public class TestConsoleEntry implements Serializable {

	private static final long serialVersionUID = -1808578272659814103L;
	private String mContents = "";
	private int mNum;
	private final ArrayList<String> mKeywords = new ArrayList<String>();

	public TestConsoleEntry(String commandParams, int entryNum) {
		mNum = entryNum;
		if (commandParams != null) {
			mContents = commandParams;
			String[] inputArr = commandParams.split(TestActivity.WHITESPACE);
			for(String word : inputArr) {
				if(CommandDispatcher.isKeyword(word)) {
					mKeywords.add(word);
				}
			}
		}
	}

	public String getContents() {
		return mContents;
	}
	
	public int getNum() {
		return mNum;
	}

	public final List<String> getKeywords() {
		return mKeywords;
	}

}