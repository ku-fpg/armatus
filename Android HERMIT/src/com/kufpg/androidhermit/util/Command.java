package com.kufpg.androidhermit.util;

public abstract class Command {

	private String mCommandName;
	private int mMinArgs;
	private boolean mLowerArgBound;
	
	public Command( String commandName, int minArgs, boolean lowerArgBound) {
		mMinArgs = minArgs;
		mLowerArgBound = lowerArgBound;
		mCommandName = commandName;
	}
	
	public String getCommandName() {
		return mCommandName;
	}
	
	public int getMinArgs() {
		return mMinArgs;
	}
	
	public boolean hasLowerArgBound() {
		return mLowerArgBound;
	}
	
	protected abstract void run(String... args);
	
}
