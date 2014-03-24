package edu.kufpg.armatus.util;

import android.widget.ExpandableListView;
import android.widget.ListView;

public final class Views {
	
	private Views() {}
	
	/**
	 * Returns whether a particular ListView entry is currently shown to the
	 * user on-screen.
	 * @param listView
	 * @param entryIndex The index of the entry to look up.
	 * @return {@code true} if the entry is currently visible to the user.
	 */
	public static boolean isEntryVisible(ListView listView, int entryIndex) {
		return listView.getFirstVisiblePosition() <= entryIndex
				&& entryIndex <= listView.getLastVisiblePosition();
	}
	
	public static int getFlatListPosition(ExpandableListView expListView, int groupPos, int childPos) {
		return expListView.getFlatListPosition(ExpandableListView.getPackedPositionForChild(groupPos, childPos));
	}
	
	public static int getGroupPosition(ExpandableListView expListView, int flatListPos) {
		return ExpandableListView.getPackedPositionGroup(expListView.getExpandableListPosition(flatListPos));
	}
	
	public static int getChildPosition(ExpandableListView expListView, int flatListPos) {
		return ExpandableListView.getPackedPositionChild(expListView.getExpandableListPosition(flatListPos));
	}
	
}
