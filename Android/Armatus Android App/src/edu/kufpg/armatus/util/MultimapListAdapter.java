package edu.kufpg.armatus.util;

import java.util.List;

import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;

import com.google.common.collect.ListMultimap;

public abstract class MultimapListAdapter<Group, Child> extends BaseExpandableListAdapter {
	private List<Group> mGroupData;
	private ListMultimap<Group, Child> mChildData;
	
	public MultimapListAdapter(List<Group> groupData, ListMultimap<Group, Child> childData) {
		mGroupData = groupData;
		mChildData = childData;
	}
	
	@Override
	public int getGroupCount() {
		return mGroupData.size();
	}

	@Override
	public int getChildrenCount(int groupPosition) {
		return mChildData.get(getGroup(groupPosition)).size();
	}

	@Override
	public Group getGroup(int groupPosition) {
		return mGroupData.get(groupPosition);
	}

	@Override
	public Child getChild(int groupPosition, int childPosition) {
		return mChildData.get(getGroup(groupPosition)).get(childPosition);
	}

	@Override
	public long getGroupId(int groupPosition) {
		return groupPosition;
	}

	@Override
	public long getChildId(int groupPosition, int childPosition) {
		return childPosition;
	}

	@Override
	public boolean hasStableIds() {
		return true;
	}

	@Override
	public abstract View getGroupView(int groupPosition, boolean isExpanded, View convertView, ViewGroup parent);

	@Override
	public abstract View getChildView(int groupPosition, int childPosition, boolean isLastChild, View convertView, ViewGroup parent);

	@Override
	public boolean isChildSelectable(int groupPosition, int childPosition) {
		return true;
	}

}
