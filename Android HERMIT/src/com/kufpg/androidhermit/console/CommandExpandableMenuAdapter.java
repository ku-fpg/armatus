package com.kufpg.androidhermit.console;

import java.util.List;
import java.util.Map;

import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.drag.DragIcon;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

public class CommandExpandableMenuAdapter extends BaseExpandableListAdapter {

	private Context mContext;
	private List<String> mGroupList;
	private Map<String, ? extends List<String>> mGroupMap;

	public CommandExpandableMenuAdapter(Context context, List<String> groupList, Map<String, ? extends List<String>> groupMap) {
		mContext = context;
		mGroupList = groupList;
		mGroupMap = groupMap;
	}

	@Override
	public Object getChild(int groupPosition, int childPosition) {
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

		String commandName = (String) getChild(groupPosition, childPosition);
		if (view == null) {
			LayoutInflater inflater = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			view = inflater.inflate(R.layout.command_expandable_child, null);
		}

		DragIcon icon = (DragIcon) view.findViewById(R.id.drag_icon);
		icon.setCommandName(commandName);

		return view;
	}

	@Override
	public int getChildrenCount(int groupPosition) {
		List<String> commandNames = mGroupMap.get(getGroup(groupPosition));
		return commandNames.size();
	}

	@Override
	public Object getGroup(int groupPosition) {
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
	public View getGroupView(int groupPosition, boolean isLastChild, View view,
			ViewGroup parent) {

		String groupName = (String) getGroup(groupPosition);
		if (view == null) {
			LayoutInflater inf = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			view = inf.inflate(R.layout.command_expandable_group, null);
		}

		TextView heading = (TextView) view.findViewById(R.id.group_heading);
		heading.setText(groupName);

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

}
