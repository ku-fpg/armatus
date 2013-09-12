package edu.kufpg.armatus.dialog;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.networking.data.CommandInfo;
import android.graphics.Color;
import android.os.Bundle;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

public class CommandHelpDialog extends ConsiderateDialog {

	private CommandInfo mCommandInfo;
	private CharSequence mTagBoxes;

	public static CommandHelpDialog newInstance(CommandInfo commandInfo) {
		CommandHelpDialog hd = new CommandHelpDialog();
		Bundle args = new Bundle();
		args.putParcelable("commandInfo", commandInfo);
		hd.setArguments(args);
		return hd;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mCommandInfo = (CommandInfo) getArguments().getParcelable("commandInfo");
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.command_help_dialog, container, false);
		getDialog().setTitle(mCommandInfo.getName());
		setCancelable(true);

		TextView commandInfoView = (TextView) v.findViewById(R.id.command_help_info_text);
		commandInfoView.setText(mCommandInfo.getHelp());
		
		TextView commandTagsView = (TextView) v.findViewById(R.id.command_help_tags_text);
		if (savedInstanceState == null) {
			SpannableStringBuilder builder = new SpannableStringBuilder();
			for (String tag : mCommandInfo.getTags()) {
				SpannableString tagBox = new SpannableString(" " + tag + " ");
				tagBox.setSpan(new BackgroundColorSpan(Color.GRAY), 0, tagBox.length(), 0);
				tagBox.setSpan(new ForegroundColorSpan(Color.BLACK), 0, tagBox.length(), 0);
				builder.append(tagBox).append(", ");
			}
			builder.delete(builder.length() - 2, builder.length());
			mTagBoxes = builder;
		} else {
			mTagBoxes = savedInstanceState.getCharSequence("tagBoxes");
		}
		commandTagsView.setText(mTagBoxes);

		return v;
	}
	
	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putCharSequence("tagBoxes", mTagBoxes);
	}

}
