package edu.kufpg.armatus.server;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.content.res.TypedArray;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSet;

import edu.kufpg.armatus.R;

public class HermitWebClient extends AbsHermitClient {

	@Override
	public void init() {
	}

	@Override
	public void doCommand() {
	}

	@Override
	public void getCommands() {
	}

	@Override
	public void reset() {
	}
	
	/**
	 * Initializes {@link #GROUP_LIST} and {@link #GROUP_TO_COMMAND_MAP} by scanning through
	 * {@code strings.xml} and reading the names of the strings.
	 * @param context The {@link Context} to use.
	 */
	protected void loadExpandableMenuData(Context context) {
		TypedArray ta = context.getResources().obtainTypedArray(R.array.command_group_arrays);
		ImmutableList.Builder<String> groupListBuilder = ImmutableList.builder();
		ImmutableMap.Builder<String, GroupData> groupDataBuilder = ImmutableMap.builder();
		ImmutableSet.Builder<String> commandSetBuilder = ImmutableSet.builder();
		for (int i = 0; i < ta.length(); i++) {
			String[] parents = context.getResources().getStringArray(R.array.command_groups);
			int id = ta.getResourceId(i, 0);
			if (id > 0) {
				String groupName = parents[i];
				String barColor = GROUP_BAR_COLORS[i - 1];
				int dragIconBackgroundId = GROUP_ICON_BACKGROUND_IDS[i - 1];
				groupListBuilder.add(groupName);

				String[] children = context.getResources().getStringArray(id);
				List<String> commandList = new ArrayList<String>();
				for (int j = 1; j < children.length; j++) { //Don't start at index 0; it's the id
					String commandName = children[j];
					commandList.add(commandName);
					commandSetBuilder.add(commandName);
				}
				groupDataBuilder.put(groupName, new GroupData(barColor,
						dragIconBackgroundId, commandList));
			}
		}
		setCommandSet(commandSetBuilder.build());
		setGroupList(groupListBuilder.build());
		setGroupDataMap(groupDataBuilder.build());
		ta.recycle();
	}

}
