package com.kufpg.armatus.console;

import android.graphics.Color;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;
import android.widget.TextView;


/**
 * Colors ConsoleEntry text depending on its usage of Keywords.
 */
public class PrettyPrinter {
	public static final String RED = "#CC060B";
	public static final String GREEN = "#1DDA1C";
	public static final String BLUE = "#0090D3";
	public static final String GRAY = "#969696";
	public static final String PURPLE = "#7300FB";
	public static final String YELLOW = "#FDFD0D";

	public static void setPrettyText(TextView tv, String text) {
		SpannableStringBuilder builder = new SpannableStringBuilder();
		for (String word : text.split(" ")) {
			if (CommandDispatcher.isKeyword(word)) {
				SpannableString spanWord = new SpannableString(word + " ");
				spanWord.setSpan(new ForegroundColorSpan(Color.parseColor(CommandDispatcher.
						getKeyword(word).getColor())), 0, word.length(), 0);
				builder.append(spanWord);
			} else {
				builder.append(word + " ");
			}
		}
		builder.delete(builder.length() - 1, builder.length()); //Trim trailing whitespace
		tv.setText(builder);
	}

}