package edu.kufpg.armatus.dialog;

import java.util.ArrayList;
import java.util.List;

import android.app.LocalActivityManager;
import android.content.Intent;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TabHost;
import android.widget.TabHost.TabSpec;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.activity.CommandHelpActivity;
import edu.kufpg.armatus.data.CommandInfo;

@SuppressWarnings("deprecation")
public class CommandHelpDialog extends ConsiderateDialog {

	private List<CommandInfo> mCommandInfos;

	public static CommandHelpDialog newInstance(List<? extends CommandInfo> commandInfos) {
		CommandHelpDialog hd = new CommandHelpDialog();
		Bundle args = new Bundle();
		args.putInt("commandInfosSize", commandInfos.size());
		for (int i = 0; i < commandInfos.size(); i++) {
			args.putParcelable("commandInfo"+i, commandInfos.get(i));
		}
		hd.setArguments(args);
		return hd;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		int commandInfosSize = getArguments().getInt("commandInfosSize");
		mCommandInfos = new ArrayList<CommandInfo>(commandInfosSize);
		for (int i = 0; i < commandInfosSize; i++) {
			mCommandInfos.add(i, (CommandInfo) getArguments().getParcelable("commandInfo"+i));
		}
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.command_help_dialog, container, false);
		getDialog().setTitle(mCommandInfos.get(0).getName());
		setCancelable(true);

		TabHost th = (TabHost) v.findViewById(android.R.id.tabhost);
		// Ugly hack that is needed to use a TabHost with Intents in a Dialog
		LocalActivityManager localActivityManager = new LocalActivityManager(getActivity(), false);
		localActivityManager.dispatchCreate(savedInstanceState);
		th.setup(localActivityManager);

		for (int i = 0; i < mCommandInfos.size(); i++) {
			TabSpec ts = th.newTabSpec("ts"+i);
			Intent intent = new Intent(getActivity(), CommandHelpActivity.class);
			CommandInfo ci = mCommandInfos.get(i);
			int ciSize = ci.getArgTypes().size();
			intent.putExtra("commandInfo", ci);
			ts.setIndicator(ciSize == 1 ? "1 arg" : (ciSize + " args")).setContent(intent);
			th.addTab(ts);
		}
		th.setCurrentTab(0);

		return v;
	}
}
