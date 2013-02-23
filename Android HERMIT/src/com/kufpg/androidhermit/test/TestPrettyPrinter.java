package com.kufpg.androidhermit.test;

import android.text.Html;
import android.widget.TextView;

import com.kufpg.androidhermit.console.CommandDispatcher;

/**
 * Colors TestConsoleEntry text depending on its usage of Keywords.
 */
public class TestPrettyPrinter {
	public static final String RED = "#CC060B";
	public static final String GREEN = "#1DDA1C";
	public static final String BLUE = "#0090D3";
	
	public static void setPrettyText(TextView tv, String text) {
		String res = "";
		//Make sure to sanitize string for HTML parsing
		String[] sentence = //TextUtils.htmlEncode(text) ;; Disable this for now; it breaks newlines
				text.split(TestActivity.WHITESPACE);
		for (String word : sentence) {
			String color = null;
			if (CommandDispatcher.isKeyword(word)) {
				color = CommandDispatcher.getKeyword(word).getColor();
				res += "<font color='" + color + "'>" +	word + "</font> ";
			} else {
				res += word + " ";
			}
		};
		res.trim();
		tv.setText(Html.fromHtml(res));
	}
}