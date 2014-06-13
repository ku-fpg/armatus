package edu.kufpg.armatus.activity;

import android.app.Activity;
import android.graphics.Color;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;
import android.widget.TextView;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.data.CommandInfo;

public class CommandHelpActivity extends Activity {

    private CommandInfo mCommandInfo;
    private CharSequence mTagBoxes;

    @Override public void onCreate(@Nullable final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.command_help_activity);

        final Bundle extras = getIntent().getExtras();
        if (extras != null) {
            mCommandInfo = extras.getParcelable("commandInfo");
        }

        final TextView commandInfoView = (TextView) findViewById(R.id.command_help_info_text);
        commandInfoView.setText(mCommandInfo.getHelp());

        final TextView commandTypesView = (TextView) findViewById(R.id.command_help_types_text);
        commandTypesView.setTypeface(ConsoleActivity.TYPEFACE);
        for (final String type : mCommandInfo.getArgTypes()) {
            commandTypesView.append(type + " → ");
        }
        commandTypesView.append(mCommandInfo.getResultType());

        final TextView commandTagsView = (TextView) findViewById(R.id.command_help_tags_text);
        if (savedInstanceState == null) {
            final SpannableStringBuilder builder = new SpannableStringBuilder();
            for (final String tag : mCommandInfo.getTags()) {
                final SpannableString tagBox = new SpannableString(' ' + tag + ' ');
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

    @Override public void onSaveInstanceState(@NonNull final Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putCharSequence("tagBoxes", mTagBoxes);
    }

    @Override public void onBackPressed() {}

}
