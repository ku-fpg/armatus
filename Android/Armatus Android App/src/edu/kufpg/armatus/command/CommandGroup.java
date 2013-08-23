package edu.kufpg.armatus.command;

import java.util.List;

public class CommandGroup {
	private final String mBarColor;
	private final int mDragIconBackgroundId;
	private final List<String> mCommandList;

	public CommandGroup(String barColor, int dragIconBackgroundId, List<String> commandList) {
		mBarColor = barColor;
		mDragIconBackgroundId = dragIconBackgroundId;
		mCommandList = commandList;
	}

	public String getBarColor() {
		return mBarColor;
	}

	public int getDragIconBackgroundId() {
		return mDragIconBackgroundId;
	}

	public List<String> getCommandList() {
		return mCommandList;
	}
}