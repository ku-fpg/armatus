package com.kufpg.armatus.console;

import java.util.List;

import com.google.common.collect.ListMultimap;
import com.kufpg.armatus.R;
import com.kufpg.armatus.drag.DragIcon;

import android.content.Context;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

public class CommandExpandableMenuAdapter extends BaseExpandableListAdapter {

	private Context mContext;
	private List<String> mGroupList;
	private ListMultimap<String, String> mGroupMap;

	public CommandExpandableMenuAdapter(Context context, List<String> groupList,
			ListMultimap<String, String> groupMap) {
		mContext = context;
		mGroupList = groupList;
		mGroupMap = groupMap;
	}

	@Override
	public String getChild(int groupPosition, int childPosition) {
		List<String> commandNames = mGroupMap.get(getGroup(groupPosition));
		return commandNames.get(childPosition);
	}

	@Override
	public long getChildId(int groupPosition, int childPosition) {
		return childPosition;
	}

	@Override
	public View getChildView(int groupPosition, int childPosition, boolean isLastChild,
			View view, ViewGroup parent) {
		String commandName = getChild(groupPosition, childPosition);
		CommandExpandableMenuItem item;
		if (view == null) {
			LayoutInflater inflater = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			view = inflater.inflate(R.layout.command_expandable_child, null);
			item = new CommandExpandableMenuItem();
			item.icon = (DragIcon) view.findViewById(R.id.drag_icon);
			view.setTag(item);
		} else {
			item = (CommandExpandableMenuItem) view.getTag();
		}

		item.icon.setCommandName(commandName);
		return view;
	}

	@Override
	public int getChildrenCount(int groupPosition) {
		List<String> commandNames = mGroupMap.get(getGroup(groupPosition));
		return commandNames.size();
	}

	@Override
	public String getGroup(int groupPosition) {
		return mGroupList.get(groupPosition);
	}

	@Override
	public int getGroupCount() {
		return mGroupList.size();
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
			LayoutInflater inf = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			view = inf.inflate(R.layout.command_expandable_group, null);
			header = new CommandExpandableMenuHeader();
			header.title = (TextView) view.findViewById(R.id.group_heading);
			header.groupColor = (View) view.findViewById(R.id.group_color);
			view.setTag(header);
		} else {
			header = (CommandExpandableMenuHeader) view.getTag();
		}

		header.title.setText(groupName);
		String colorHex = CommandDispatcher.getGroupColor(groupName);
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

	private static class CommandExpandableMenuHeader {
		TextView title;
		View groupColor;
	}

	private static class CommandExpandableMenuItem {
		DragIcon icon;
	}

}
