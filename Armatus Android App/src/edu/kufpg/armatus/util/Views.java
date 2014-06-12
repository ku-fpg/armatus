package edu.kufpg.armatus.util;

import android.support.annotation.NonNull;
import android.widget.ExpandableListView;
import android.widget.ListView;

public final class Views {

    private Views() {
    }

    /**
     * Returns whether a particular ListView entry is currently shown to the
     * user on-screen.
     *
     * @param listView
     * @param entryIndex The index of the entry to look up.
     * @return {@code true} if the entry is currently visible to the user.
     */
    public static boolean isEntryVisible(@NonNull final ListView listView, final int entryIndex) {
        return listView.getFirstVisiblePosition() <= entryIndex
                && entryIndex <= listView.getLastVisiblePosition();
    }

    public static int getFlatListPosition(@NonNull final ExpandableListView expListView, final int groupPos, final int childPos) {
        return expListView.getFlatListPosition(ExpandableListView.getPackedPositionForChild(groupPos, childPos));
    }

    public static int getGroupPosition(@NonNull final ExpandableListView expListView, final int flatListPos) {
        return ExpandableListView.getPackedPositionGroup(expListView.getExpandableListPosition(flatListPos));
    }

    public static int getChildPosition(@NonNull final ExpandableListView expListView, final int flatListPos) {
        return ExpandableListView.getPackedPositionChild(expListView.getExpandableListPosition(flatListPos));
    }

}
