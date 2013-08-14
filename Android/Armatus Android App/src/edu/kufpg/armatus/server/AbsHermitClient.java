package edu.kufpg.armatus.server;

import java.util.List;
import java.util.Map;
import java.util.Set;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.PrettyPrinter;

public abstract class AbsHermitClient implements HermitClient {
	protected static final String[] GROUP_BAR_COLORS = { PrettyPrinter.RED, PrettyPrinter.BLUE,
		PrettyPrinter.YELLOW, PrettyPrinter.GREEN, PrettyPrinter.GRAY };

	protected static final int[] GROUP_ICON_BACKGROUND_IDS = { R.drawable.template_red, R.drawable.template_blue,
		R.drawable.template_yellow, R.drawable.template_green, R.drawable.template_green,
		R.drawable.template_gray };
	
	private Set<String> mServerDefinedCommandSet;
	private Set<String> mClientDefinedCommandSet;
	
	/** Contains all {@link edu.kufpg.armatus.console.CommandDispatcher.Command Command}
	 * group names in the order displayed in the list. */
	private List<String> mGroupList;

	private Map<String, GroupData> mGroupDataMap;
	
	protected List<String> getGroupList() {
		return mGroupList;
	}
	
	public List<String> getGroupCommands(String groupName) {
		return mGroupDataMap.get(groupName).commandList;
	}
	
	protected Map<String, GroupData> getGroupDataMap() {
		return mGroupDataMap;
	}
	
	public boolean isCommand(String commandName) {
		return mServerDefinedCommandSet.contains(commandName);
	}
	
	protected void setCommandSet(Set<String> commandSet) {
		mServerDefinedCommandSet = commandSet;
	}
	
	protected void setGroupList(List<String> groupList) {
		mGroupList = groupList;
	}
	
	protected void setGroupDataMap(Map<String, GroupData> groupDataMap) {
		mGroupDataMap = groupDataMap;
	}

	public static class GroupData {
		public final String barColor;
		public final int dragIconBackgroundId;
		public final List<String> commandList;

		public GroupData(String barColor, int dragIconBackgroundId, List<String> commandList) {
			this.barColor = barColor;
			this.dragIconBackgroundId = dragIconBackgroundId;
			this.commandList = commandList;
		}
	}

	@Override
	public abstract void init();

	@Override
	public abstract void doCommand();

	@Override
	public abstract void getCommands();

	@Override
	public abstract void reset();

}
