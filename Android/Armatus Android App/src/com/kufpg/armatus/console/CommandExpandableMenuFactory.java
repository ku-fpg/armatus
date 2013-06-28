package com.kufpg.armatus.console;

import java.util.List;

import android.content.Context;
import android.content.res.TypedArray;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ListMultimap;
import com.kufpg.armatus.R;

public class CommandExpandableMenuFactory {

	private static List<String> GROUP_LIST;
	private static ListMultimap<String, String> GROUP_TO_COMMAND_MAP;
	private static boolean mInitialized = false;

	public static List<String> getGroupList(Context context) {
		if (!mInitialized) {
			loadExpandableMenuData(context);
			mInitialized = true;
		}
		return GROUP_LIST;
	}

	public static ListMultimap<String, String> getGroupMap(Context context) {
		if (!mInitialized) {
			loadExpandableMenuData(context);
			mInitialized = true;
		}
		return GROUP_TO_COMMAND_MAP;
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
