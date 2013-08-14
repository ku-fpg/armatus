package edu.kufpg.armatus.console;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.command.CommandDispatcher;
import edu.kufpg.armatus.drag.DragIcon;
import android.content.Context;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

/**
 * {@link android.widget.ExpandableListAdapter ExpandableListAdapter} for a menu containing
 * {@link DragIcon}s representing various console {@link
 * edu.kufpg.armatus.console.CommandDispatcher.Command Command}s.
 */
public class CommandExpandableMenuAdapter extends BaseExpandableListAdapter {
	private Context mContext;
	private LayoutInflater mInflater;

	/**
	 * Constructs a new instance and initializes the menu data if necessary.
	 * @param context The {@link Context} to use.
	 */
	public CommandExpandableMenuAdapter(Context context) {
		mContext = context;
		mInflater = LayoutInflater.from(context);
	}

	@Override
	public String getChild(int groupPosition, int childPosition) {
		return CommandExpandableMenu.getGroupMetadataMap(mContext)
				.get(getGroup(groupPosition)).commandList.get(childPosition);
	}

	@Override
	public long getChildId(int groupPosition, int childPosition) {
		return childPosition;
	}

	@Override
	public View getChildView(int groupPosition, int childPosition, boolean isLastChild,
			View view, ViewGroup parent) {
		CommandExpandableMenuItem item;
		if (view == null) {
			view = mInflater.inflate(R.layout.command_expandable_child, null);
			item = new CommandExpandableMenuItem();
			item.icon = (DragIcon) view.findViewById(R.id.drag_icon);
			view.setTag(item);
		} else {
			item = (CommandExpandableMenuItem) view.getTag();
		}

		String commandName = getChild(groupPosition, childPosition);
		String groupName = getGroup(groupPosition);
		item.icon.setBackgroundResource(CommandExpandableMenu.getGroupMetadataMap(mContext)
				.get(groupName).dragIconBackgroundId);
		item.icon.setText(commandName);
		item.icon.setGroupName(groupName);
		return view;
	}

	@Override
	public int getChildrenCount(int groupPosition) {
		return CommandExpandableMenu.getGroupMetadataMap(mContext)
				.get(getGroup(groupPosition)).commandList.size();
	}

	@Override
	public String getGroup(int groupPosition) {
		return CommandExpandableMenu.getGroupList(mContext).get(groupPosition);
	}

	@Override
	public int getGroupCount() {
		return CommandExpandableMenu.getGroupList(mContext).size();
	}

	@Override
	public long getGroupId(int groupPosition) {
		return groupPosition;
	}

	@Override
	public View getGroupView(int groupPosition, boolean isLastChild, View view, ViewGroup parent) {
		String groupName = getGroup(groupPosition);
		CommandExpandableMenuHeader header;
		if (view == null) {
			view = mInflater.inflate(R.layout.command_expandable_group, null);
			header = new CommandExpandableMenuHeader();
			header.title = (TextView) view.findViewById(R.id.group_heading);
			header.groupColor = (View) view.findViewById(R.id.group_color);
			view.setTag(header);
		} else {
			header = (CommandExpandableMenuHeader) view.getTag();
		}

		header.title.setText(groupName);
		String colorHex = CommandExpandableMenu.getGroupMetadataMap(mContext).get(groupName).barColor;
		if (colorHex == null) {
			colorHex = PrettyPrinter.GRAY;
		}
		header.groupColor.setBackgroundColor(Color.parseColor(colorHex));

		return view;
	}

	@Override
	public boolean hasStableIds() {
		return true;
	}

	@Override
	public boolean isChildSelectable(int groupPosition, int childPosition) {
		return true;
	}

	/** Holds group title information for efficiency purposes. */
	private static class CommandExpandableMenuHeader {
		TextView title;
		View groupColor;
	}

	/** Holds a {@link DragIcon} reference for efficiency purposes. */
	private static class CommandExpandableMenuItem {
		DragIcon icon;
	}

}
