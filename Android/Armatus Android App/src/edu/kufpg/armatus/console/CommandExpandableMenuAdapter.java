package edu.kufpg.armatus.console;

import java.util.List;
import java.util.Locale;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseExpandableListAdapter;
import android.widget.Filter;
import android.widget.Filterable;
import android.widget.TextView;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Lists;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.data.CommandInfo;

/**
 * {@link android.widget.ExpandableListAdapter ExpandableListAdapter} for a menu containing
 * {@link DragIcon}s representing various console {@link
 * edu.kufpg.armatus.console.CustomCommandInfo.CommandInfo Command}s.
 */
public class CommandExpandableMenuAdapter extends BaseExpandableListAdapter implements Filterable {
	private final LayoutInflater mInflater;
	private Filter mFilter;
	private CharSequence mConstraint;
	private final Object mLock = new Object();
	private List<String> mTags;
	private ListMultimap<String, String> mTagCommandNames;

	/**
	 * Constructs a new instance and initializes the menu data if necessary.
	 * @param context The {@link Context} to use.
	 */
	public CommandExpandableMenuAdapter(Context context) {
		mInflater = LayoutInflater.from(context);
		resetData();
	}

	public CommandExpandableMenuAdapter(Context context, CharSequence searchConstraint) {
		this(context);
		getFilter().filter(searchConstraint);
	}

	@Override
	public String getChild(int groupPosition, int childPosition) {
		return mTagCommandNames.get(getGroup(groupPosition)).get(childPosition);
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
			item.commandName = (TextView) view.findViewById(R.id.command_expandable_command_name);
			item.typeSigs = (TextView) view.findViewById(R.id.command_expandable_type_sigs);
			view.setTag(item);
		} else {
			item = (CommandExpandableMenuItem) view.getTag();
		}

		String commandName = getChild(groupPosition, childPosition);
		int typeSigs = CommandHolder.getCommandTypeSigCount(commandName);
		item.commandName.setText(commandName);
		item.commandName.setTypeface(ConsoleActivity.TYPEFACE);
		item.typeSigs.setText(String.valueOf(typeSigs));

		return view;
	}

	@Override
	public int getChildrenCount(int groupPosition) {
		return mTagCommandNames.get(getGroup(groupPosition)).size();
	}

	@Override
	public String getGroup(int groupPosition) {
		return mTags.get(groupPosition);
	}

	@Override
	public int getGroupCount() {
		return mTags.size();
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

	@Override
	public void notifyDataSetChanged() {
		if (mConstraint == null) {
			resetData();
			notifyDataSetChangedInternal();
		} else {
			getFilter().filter(mConstraint);
		}
	}

	private void notifyDataSetChangedInternal() {
		super.notifyDataSetChanged();
	}

	/** Holds group title information for efficiency purposes. */
	static class CommandExpandableMenuHeader {
		TextView title;
	}

	/** Holds a {@link DragIcon} reference for efficiency purposes. */
	static class CommandExpandableMenuItem {
		TextView commandName, typeSigs;
	}

	private static class MenuDataHolder {
		List<String> tags;
		ListMultimap<String, String> tagCommandNames;
	}

	private void resetData() {
		mTags = CommandHolder.getTags();
		mTagCommandNames = CommandHolder.getTagCommandNames();
	}

	@Override
	public Filter getFilter() {
		if (mFilter == null) {
			mFilter = new Filter() {
				@Override
				protected FilterResults performFiltering(CharSequence constraint) {
					mConstraint = constraint;
					FilterResults results = new FilterResults();
					MenuDataHolder holder;
					synchronized (mLock) {
						holder = new MenuDataHolder();
					}

					if (constraint == null) {
						holder.tags = CommandHolder.getTags();
						holder.tagCommandNames = CommandHolder.getTagCommandNames();
						results.values = holder;
					} else {
						String search = constraint.toString().toLowerCase(Locale.US);
						List<String> newTags;
						ListMultimap<String, String> newTagCmdNames;
						synchronized (mLock) {
							newTags = Lists.newArrayList();
							newTagCmdNames = ArrayListMultimap.create();
						}

						for (final String tag : CommandHolder.getTags()) {
							final String ltag = tag.toLowerCase(Locale.US);
							if (ltag.contains(search)) {
								newTags.add(tag);
								newTagCmdNames.putAll(tag, CommandHolder.getCommandNamesFromTag(tag));
							} else {
								for (final String name : CommandHolder.getCommandNamesFromTag(tag)) {
									final String lname = name.toLowerCase(Locale.US);
									if (lname.contains(search)) {
										newTagCmdNames.put(tag, name);
									} else {
										List<? extends CommandInfo> cmds = CommandHolder.getCommandsFromName(name);
										cmdLoop: for (CommandInfo cmd : cmds) {
											final String help = cmd.getHelp().toLowerCase(Locale.US);
											if (help.contains(search)) {
												newTagCmdNames.put(tag, name);
												break cmdLoop;
											}

											final String cmdName = cmd.getName().toLowerCase(Locale.US);
											if (cmdName.contains(search)) {
												newTagCmdNames.put(tag, name);
												break cmdLoop;
											}

											final List<String> tags = cmd.getTags();
											for (final String innerTag : tags) {
												final String linnerTag = innerTag.toLowerCase(Locale.US);
												if (linnerTag.contains(search)) {
													newTagCmdNames.put(tag, name);
													break cmdLoop;
												}
											}

											final List<String> argTys = cmd.getArgTypes();
											for (final String argTy : argTys) {
												final String largTy = argTy.toLowerCase(Locale.US);
												if (largTy.contains(search)) {
													newTagCmdNames.put(tag, name);
													break cmdLoop;
												}
											}

											final String resTy = cmd.getResultType().toLowerCase(Locale.US);
											if (resTy.contains(search)) {
												newTagCmdNames.put(tag, name);
												break cmdLoop;
											}
										}
									}
								}

								if (newTagCmdNames.containsKey(tag)) {
									newTags.add(tag);
								}
							}
						}

						holder.tags = newTags;
						holder.tagCommandNames = newTagCmdNames;
						results.values = holder;
					}
					return results;
				}

				@Override
				protected void publishResults(CharSequence constraint,
						FilterResults results) {
					MenuDataHolder holder = (MenuDataHolder) results.values;
					mTags = holder.tags;
					mTagCommandNames = holder.tagCommandNames;
					notifyDataSetChangedInternal();
				}
			};
		}
		return mFilter;
	}

}
