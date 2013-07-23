package com.kufpg.armatus.console;

import com.kufpg.armatus.util.StringUtils;

import android.graphics.Color;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;
import android.util.Log;
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
		Log.d("TESTTEST", "PrettyPrinting engage!");
		String sentenceStr = text.trim();
		String leadSpace = StringUtils.getLeadingWhitespace(text);
		String trailSpace = StringUtils.getTrailingWhitespace(text);
		String[] sentenceArr = sentenceStr.split(StringUtils.WHITESPACE);
		SpannableStringBuilder builder = new SpannableStringBuilder(leadSpace);
		
		int index = 0;
		for (String word : sentenceArr) {
			index += word.length();
			String followingSpace = StringUtils.getLeadingWhitespace(sentenceStr.substring(index));
			index += followingSpace.length();
			if (CommandDispatcher.isKeyword(word)) {
				SpannableString spanWord = new SpannableString(word + followingSpace);
				spanWord.setSpan(new ForegroundColorSpan(Color.parseColor(CommandDispatcher.
						getKeyword(word).getColor())), 0, word.length(), 0);
				builder.append(spanWord);
			} else {
				builder.append(word + followingSpace);
			}
		}
		
		tv.setText(builder.append(trailSpace));
	}

}