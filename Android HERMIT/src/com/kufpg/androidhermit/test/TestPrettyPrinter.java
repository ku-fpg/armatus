package com.kufpg.androidhermit.test;

import android.text.Editable;
import android.text.Html;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.widget.TextView;

import com.kufpg.androidhermit.console.CommandDispatcher;

public class TestPrettyPrinter implements TextWatcher {
	public static final String RED = "#CC060B";
	public static final String GREEN = "#1DDA1C";
	public static final String BLUE = "#0090D3";
	private String mLastText;
	private TextView mPrettyView;

	public TestPrettyPrinter(TextView prettyView) {
		mPrettyView = prettyView;
	}
	
	@Override
	public void onTextChanged(CharSequence s, int start,
			int before, int count) {
		if (!s.toString().equals(mLastText)) {
			mLastText = s.toString();

			String res = "";
			//Make sure to sanitize string for HTML parsing
			String[] sentence = TextUtils.htmlEncode(s.toString())
					.split(TestActivity.WHITESPACE);
			for (String word : sentence) {
				String color = null;
				if (CommandDispatcher.isKeyword(word)) {
					color = CommandDispatcher.getKeyword(word).getColor();
					res += "<font color='" + color + "'>" +	word + "</font> ";
				} else {
					res += word + " ";
				}
			};
			mPrettyView.setText(Html.fromHtml(res));
		}
	}

	@Override
	public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

	@Override
	public void afterTextChanged(Editable s) {}
}