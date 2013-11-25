package edu.kufpg.armatus.activity;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.data.CommandInfo;
import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;
import android.widget.TextView;

public class CommandHelpActivity extends Activity {

	private CommandInfo mCommandInfo;
	private CharSequence mTagBoxes;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.command_help_activity);

		Bundle extras = getIntent().getExtras();
		if (extras != null) {
			mCommandInfo = extras.getParcelable("commandInfo");
		}

		TextView commandInfoView = (TextView) findViewById(R.id.command_help_info_text);
		commandInfoView.setText(mCommandInfo.getHelp());

		TextView commandTypesView = (TextView) findViewById(R.id.command_help_types_text);
		commandTypesView.setTypeface(ConsoleActivity.TYPEFACE);
		for (String type : mCommandInfo.getArgTypes()) {
			commandTypesView.append(type + " → ");
		}
		commandTypesView.append(mCommandInfo.getResultType());

		TextView commandTagsView = (TextView) findViewById(R.id.command_help_tags_text);
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
	}

	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putCharSequence("tagBoxes", mTagBoxes);
	}

	@Override
	public void onBackPressed() {}

}
