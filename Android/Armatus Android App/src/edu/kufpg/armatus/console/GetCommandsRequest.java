package edu.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.res.TypedArray;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.command.CommandDispatcher;
import edu.kufpg.armatus.command.CommandGroup;
import edu.kufpg.armatus.server.HermitWebServerRequest;

public class GetCommandsRequest extends HermitWebServerRequest<String> {
	private static final String[] GROUP_BAR_COLORS = { PrettyPrinter.RED, PrettyPrinter.BLUE,
		PrettyPrinter.YELLOW, PrettyPrinter.GREEN, PrettyPrinter.PURPLE };

	private static final int[] GROUP_ICON_BACKGROUND_IDS = { R.drawable.template_red, R.drawable.template_blue,
		R.drawable.template_yellow, R.drawable.template_green, R.drawable.template_purple };

	private ProgressDialog mProgress;

	public GetCommandsRequest(ConsoleActivity console) {
		super(console);
	}

	@Override
	protected void onPreExecute() {
		super.onPreExecute();

		getActivity().setProgressBarVisibility(false);
		showProgressDialog();
	}

	@Override
	protected void onActivityDetached() {
		if (mProgress != null) {
			mProgress.dismiss();
			mProgress = null;
		}
	}

	@Override
	protected void onActivityAttached() {
		if (mProgress == null) {
			showProgressDialog();
		}
	}

	@Override
	protected String doInBackground(String... params) {
		return null;
	}

	@Override
	protected void onPostExecute(String result) {
		super.onPostExecute(result);

		if (mProgress != null) {
			mProgress.dismiss();
		}
		loadExpandableMenuData();
	}

	private void showProgressDialog() {
		mProgress = new ProgressDialog(getActivity());
		mProgress.setProgressStyle(ProgressDialog.STYLE_SPINNER);
		mProgress.setMessage("Fetching commands...");
		mProgress.setCancelable(false);
		mProgress.show();
	}

	/**
	 * Initializes {@link #GROUP_LIST} and {@link #GROUP_TO_COMMAND_MAP} by scanning through
	 * {@code strings.xml} and reading the names of the strings.
	 * @param console The {@link Context} to use.
	 */
	private void loadExpandableMenuData() {
		TypedArray ta = getActivity().getResources().obtainTypedArray(R.array.command_group_arrays);
		ImmutableList.Builder<String> groupListBuilder = ImmutableList.builder();
		ImmutableMap.Builder<String, CommandGroup> groupDataBuilder = ImmutableMap.builder();
		ImmutableSortedSet.Builder<String> commandSetBuilder = ImmutableSortedSet.naturalOrder();
		for (int i = 0; i < ta.length(); i++) {
			String[] parents = getActivity().getResources().getStringArray(R.array.command_groups);
			int id = ta.getResourceId(i, 0);
			if (id > 0) {
				String groupName = parents[i];
				String barColor = GROUP_BAR_COLORS[i];
				int dragIconBackgroundId = GROUP_ICON_BACKGROUND_IDS[i];
				groupListBuilder.add(groupName);

				String[] children = getActivity().getResources().getStringArray(id);
				List<String> commandList = new ArrayList<String>();
				for (int j = 1; j < children.length; j++) { //Don't start at index 0; it's the id
					String commandName = children[j];
					commandList.add(commandName);
					commandSetBuilder.add(commandName);
				}
				groupDataBuilder.put(groupName, new CommandGroup(barColor,
						dragIconBackgroundId, commandList));
			}
		}
		commandSetBuilder.addAll(CommandDispatcher.getClientCommandNames());
		groupListBuilder.add(CommandDispatcher.CLIENT_COMMANDS_GROUP);
		groupDataBuilder.put(CommandDispatcher.CLIENT_COMMANDS_GROUP, new CommandGroup(PrettyPrinter.GRAY,
				R.drawable.template_gray, new ArrayList<String>(CommandDispatcher.getClientCommandNames())));

		getActivity().initCommandRelatedVariables(commandSetBuilder.build(),
				groupListBuilder.build(), groupDataBuilder.build());
		ta.recycle();
	}

}
