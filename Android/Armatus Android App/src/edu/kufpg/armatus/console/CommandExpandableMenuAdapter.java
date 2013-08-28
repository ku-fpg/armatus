package edu.kufpg.armatus.console;

import java.util.List;

import com.google.common.collect.ListMultimap;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.drag.DragIcon;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

/**
 * {@link android.widget.ExpandableListAdapter ExpandableListAdapter} for a menu containing
 * {@link DragIcon}s representing various console {@link
 * edu.kufpg.armatus.console.CustomCommand.Command Command}s.
 */
public class CommandExpandableMenuAdapter extends BaseExpandableListAdapter {
	private static List<String> GROUP_LIST;
	private static ListMultimap<String, String> TAG_MAP;
	private Context mContext;
	private LayoutInflater mInflater;

	/**
	 * Constructs a new instance and initializes the menu data if necessary.
	 * @param context The {@link Context} to use.
	 */
	public CommandExpandableMenuAdapter(Context context, List<String> groupList,
			ListMultimap<String, String> tagMap) {
		mContext = context;
		mInflater = LayoutInflater.from(context);
		if (GROUP_LIST == null) {
			GROUP_LIST = groupList;
		}
		if (TAG_MAP == null) {
			TAG_MAP = tagMap;
		}
	}

	@Override
	public String getChild(int groupPosition, int childPosition) {
		return TAG_MAP.get(getGroup(groupPosition)).get(childPosition);
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
		item.icon.setText(commandName);
		item.icon.setTypeface(ConsoleActivity.TYPEFACE);
		int newWidth = mContext.getResources().getDrawable(R.drawable.template_white).getIntrinsicWidth();
		item.icon.getLayoutParams().width = newWidth;
		return view;
	}

	@Override
	public int getChildrenCount(int groupPosition) {
		return TAG_MAP.get(getGroup(groupPosition)).size();
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
		String tagName = getGroup(groupPosition);
		CommandExpandableMenuHeader header;
		if (view == null) {
			view = mInflater.inflate(R.layout.command_expandable_group, null);
			header = new CommandExpandableMenuHeader();
			header.title = (TextView) view.findViewById(R.id.group_heading);
			view.setTag(header);
		} else {
			header = (CommandExpandableMenuHeader) view.getTag();
		}

		header.title.setText(tagName);

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
	}

	/** Holds a {@link DragIcon} reference for efficiency purposes. */
	private static class CommandExpandableMenuItem {
		DragIcon icon;
	}

}
