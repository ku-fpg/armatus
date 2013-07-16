package com.kufpg.armatus.console;

import java.util.List;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ListMultimap;
import com.kufpg.armatus.R;
import com.kufpg.armatus.drag.DragIcon;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

public class CommandExpandableMenuAdapter extends BaseExpandableListAdapter {
	private static List<String> GROUP_LIST;
	private static ListMultimap<String, String> GROUP_TO_COMMAND_MAP;
	private Context mContext;

	public CommandExpandableMenuAdapter(Context context) {
		mContext = context;
		if (GROUP_LIST == null || GROUP_TO_COMMAND_MAP == null) {
			loadExpandableMenuData(mContext);
		}
	}

	@Override
	public String getChild(int groupPosition, int childPosition) {
		List<String> commandNames = GROUP_TO_COMMAND_MAP.get(getGroup(groupPosition));
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
		List<String> commandNames = GROUP_TO_COMMAND_MAP.get(getGroup(groupPosition));
		return commandNames.size();
	}

	@Override
	public String getGroup(int groupPosition) {
		return GROUP_LIST.get(groupPosition);
	}

	@Override
	public int getGroupCount() {
		return GROUP_LIST.size();
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

	private static void loadExpandableMenuData(Context context) {
		TypedArray ta = context.getResources().obtainTypedArray(R.array.command_group_arrays);
		ImmutableList.Builder<String> groupListBuilder = ImmutableList.builder();
		ImmutableListMultimap.Builder<String, String> groupMapBuilder = ImmutableListMultimap.builder();
		for (int i = 0; i < ta.length(); i++) {
			String[] parents = context.getResources().getStringArray(R.array.command_groups);
			int id = ta.getResourceId(i, 0);
			if (id > 0) {
				String groupName = parents[i];
				groupListBuilder.add(groupName);

				String[] children = context.getResources().getStringArray(id);
				for (int j = 1; j < children.length; j++) { //Don't start at index 0; it's the id
				String commandName = children[j];
				if (CommandDispatcher.isAlias(commandName)) {
					commandName = CommandDispatcher.unaliasCommand(commandName);
				}
				groupMapBuilder.put(groupName, commandName);
				}
			}
		}
		GROUP_LIST = groupListBuilder.build();
		GROUP_TO_COMMAND_MAP = groupMapBuilder.build();
		ta.recycle();
	}

}
