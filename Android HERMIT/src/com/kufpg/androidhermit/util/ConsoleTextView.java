package com.kufpg.androidhermit.util;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.text.Editable;
import android.text.Html;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.util.Log;
import android.view.Gravity;
import android.widget.TextView;

public class ConsoleTextView extends TextView {

	public final static float TEXT_SIZE = 15;
	public final static String TYPEFACE = "fonts/DroidSansMonoDotted.ttf";

	public ConsoleTextView(Context context) {
		super(context);
		setupView(context, null);
	}

	public ConsoleTextView(Context context, AttributeSet attrs) {
		super(context, attrs);
		setupView(context, null);
	}

	public ConsoleTextView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		setupView(context, null);
	}

	public ConsoleTextView(Context context, String msg) {
		super(context);
		setupView(context, msg);
	}

	protected void setupView(Context context, String msg) {
		this.addTextChangedListener(new PrettyPrinter());
		Typeface mTypeface = Typeface.createFromAsset(context.getAssets(), TYPEFACE);
		this.setTypeface(mTypeface);
		//TODO: Create pretty-printing method
		this.setTextColor(Color.WHITE);
		this.setTextSize(TEXT_SIZE);
		this.setGravity(Gravity.BOTTOM);
		// TODO: Make a better ID system
		this.setId((int) System.currentTimeMillis());
		if (msg != null)
			this.setText(msg);
		else
			this.setText("");
	}

	protected class PrettyPrinter implements TextWatcher {
		String lastText = null;

		@Override
		public void onTextChanged(CharSequence s, int start, int before,
				int count) {
			Log.d("", "lastText='" + lastText + "'");
			Log.d("", "s='" + s + "'");
			if (!s.toString().equals(lastText)) {
				lastText = s.toString();

				String res = "";
				char[] split = s.toString().toCharArray();
				for (char c : split) {
					String color = null;
					if (c == '1' || c == '2' | c == '3') {
						color = "#CC060B"; //red
					} else if (c == '4' || c == '5' || c == '6') {
						color = "#1DDA1C"; //green
					} else if (c == '7' || c == '8' || c == '9' || c == '0') {
						color = "#0090D3"; //blue
					}
					// etc...
					if (color != null) {
						res += "<font color=\"" + color + "\">" + c
								+ "</font>";
					} else {
						res += c;
					}
				}

				ConsoleTextView.this.setText(Html.fromHtml(res));
			}
		}

		@Override
		public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

		@Override
		public void afterTextChanged(Editable s) {}

	}

}
