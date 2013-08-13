package edu.kufpg.armatus.console;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ListMultimap;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.drag.DragIcon;

import android.content.Context;
import android.content.res.TypedArray;
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
	/** Contains all {@link edu.kufpg.armatus.console.CommandDispatcher.Command Command}
	 * group names in the order displayed in the list. */
	private static List<String> GROUP_LIST;
	
	/** Maps {@link edu.kufpg.armatus.console.CommandDispatcher.Command Command} group
	 * names to the {@code Command} names belonging to each group in the order specified
	 * by {@code strings.xml}. */
	private static ListMultimap<String, String> GROUP_TO_COMMAND_MAP;
	
	/** Reference to the current context. */
	private Context mContext;
	private Map<String,Integer> colorToImageMap = new HashMap<String, Integer>();

	/**
	 * Constructs a new instance and initializes the menu data if necessary.
	 * @param context The {@link Context} to use.
	 */
	public CommandExpandableMenuAdapter(Context context) {
		mContext = context;
		
		if (GROUP_LIST == null || GROUP_TO_COMMAND_MAP == null) {
			loadExpandableMenuData(mContext);
		}
		
		colorToImageMap.put(PrettyPrinter.RED, R.drawable.template_red);
		colorToImageMap.put(PrettyPrinter.BLUE, R.drawable.template_blue);
		colorToImageMap.put(PrettyPrinter.YELLOW, R.drawable.template_yellow);
		colorToImageMap.put(PrettyPrinter.GREEN,R.drawable.template_green);
		colorToImageMap.put(PrettyPrinter.PURPLE,R.drawable.template_purple);
		colorToImageMap.put(PrettyPrinter.GRAY, R.drawable.template_gray);
		
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
		String groupName = getGroup(groupPosition);
		if (view == null) {
			LayoutInflater inflater = (LayoutInflater) mContext.getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			view = inflater.inflate(R.layout.command_expandable_child, null);
			item = new CommandExpandableMenuItem();
			item.icon = (DragIcon) view.findViewById(R.id.drag_icon);
			view.setTag(item);
		} else {
			item = (CommandExpandableMenuItem) view.getTag();
		}

		item.icon.setText(commandName);
		item.icon.setBackgroundResource(colorToImageMap.get(CommandDispatcher.getGroupColor(groupName)));
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

	/** Holds group title information for efficiency purposes. */
	private static class CommandExpandableMenuHeader {
		TextView title;
		View groupColor;
	}

	/** Holds a {@link DragIcon} reference for efficiency purposes. */
	private static class CommandExpandableMenuItem {
		DragIcon icon;
	}

	/**
	 * Initializes {@link #GROUP_LIST} and {@link #GROUP_TO_COMMAND_MAP} by scanning through
	 * {@code strings.xml} and reading the names of the strings.
	 * @param context The {@link Context} to use.
	 */
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
