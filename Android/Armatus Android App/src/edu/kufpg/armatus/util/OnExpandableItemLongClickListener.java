package edu.kufpg.armatus.util;

import android.view.View;
import android.widget.AdapterView;
import android.widget.ExpandableListView;
import android.widget.AdapterView.OnItemLongClickListener;

public abstract class OnExpandableItemLongClickListener implements OnItemLongClickListener {

	@Override
	public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
		if (parent instanceof ExpandableListView) {
			ExpandableListView elv = (ExpandableListView) parent;
			int itemType = ExpandableListView.getPackedPositionType(id);

			switch (itemType) {
			case ExpandableListView.PACKED_POSITION_TYPE_CHILD: {
				int childPosition = ExpandableListView.getPackedPositionChild(id);
				int groupPosition = ExpandableListView.getPackedPositionGroup(id);
				return onChildLongClick(elv, view, groupPosition, childPosition, id);
			}
			case ExpandableListView.PACKED_POSITION_TYPE_GROUP: {
				int groupPosition = ExpandableListView.getPackedPositionGroup(id);
				return onGroupLongClick(elv, view, groupPosition, id);
			}
			default:
				return false;
			}
		} else {
			throw new IllegalStateException("This listener can only be used with ExpandableListViews.");
		}
	}

	public boolean onChildLongClick(ExpandableListView parent, View v, int groupPosition, int childPosition, long id) {
		return false;
	}

	public boolean onGroupLongClick(ExpandableListView parent, View v, int groupPosition, long id) {
		return false;
	}

}
