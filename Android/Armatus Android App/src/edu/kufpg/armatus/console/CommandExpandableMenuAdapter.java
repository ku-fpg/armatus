package edu.kufpg.armatus.console;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.data.CommandInfo;
import edu.kufpg.armatus.dialog.CommandHelpDialog;
import edu.kufpg.armatus.drag.DragIcon;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.TextView;

/**
 * {@link android.widget.ExpandableListAdapter ExpandableListAdapter} for a menu containing
 * {@link DragIcon}s representing various console {@link
 * edu.kufpg.armatus.console.CustomCommandInfo.CommandInfo Command}s.
 */
public class CommandExpandableMenuAdapter extends BaseExpandableListAdapter {
	private ConsoleActivity mConsole;
	private LayoutInflater mInflater;
	private OnClickListener mIconClickListener;

	/**
	 * Constructs a new instance and initializes the menu data if necessary.
	 * @param console The {@link Context} to use.
	 */
	public CommandExpandableMenuAdapter(ConsoleActivity console) {
		mConsole = console;
		mInflater = LayoutInflater.from(console);
		mIconClickListener = new OnClickListener() {
			@Override
			public void onClick(View v) {
				CommandInfo info = ((DragIcon) v).getCommandInfo();
				CommandHelpDialog helpDialog = CommandHelpDialog.newInstance(info);
				helpDialog.show(mConsole.getFragmentManager(), "commandHelp");
			}
		};
	}

	@Override
	public CommandInfo getChild(int groupPosition, int childPosition) {
		return CommandHolder.getTagCommands(getGroup(groupPosition)).get(childPosition);
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
			item.icon = (DragIcon) view.findViewById(R.id.command_drag_icon);
			view.setTag(item);
		} else {
			item = (CommandExpandableMenuItem) view.getTag();
		}

		CommandInfo commandInfo = getChild(groupPosition, childPosition);
		item.icon.setCommandInfo(commandInfo);
		item.icon.setOnClickListener(mIconClickListener);
		item.icon.setTypeface(ConsoleActivity.TYPEFACE);
		int newWidth = mConsole.getResources().getDrawable(R.drawable.template_white).getIntrinsicWidth();
		item.icon.getLayoutParams().width = newWidth;
		return view;
	}

	@Override
	public int getChildrenCount(int groupPosition) {
		return CommandHolder.getTagCommands(getGroup(groupPosition)).size();
	}

	@Override
	public String getGroup(int groupPosition) {
		return CommandHolder.getTag(groupPosition);
	}

	@Override
	public int getGroupCount() {
		return CommandHolder.getTagCount();
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
