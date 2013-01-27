package com.kufpg.androidhermit.util;

import java.io.Serializable;
import java.util.ArrayList;

import org.xml.sax.XMLReader;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.graphics.drawable.Drawable;
import android.text.Editable;
import android.text.Html;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.Gravity;
import android.widget.TextView;
import android.widget.Toast;

public class ConsoleTextView extends TextView implements Serializable {

	private static final long serialVersionUID = 492620301229198361L;
	private static final String RED = "red";
	private static final String BLUE = "blue";
	private static final String GREEN = "green";

	public final static float TEXT_SIZE = 15;
	public final static String TYPEFACE = "fonts/DroidSansMonoDotted.ttf";

	private int mCommandOrderNum;

	public ConsoleTextView(Context context) {
		super(context);
		setupView(context, null, 0);
	}

	public ConsoleTextView(Context context, AttributeSet attrs) {
		super(context, attrs);
		setupView(context, null, 0);
	}

	public ConsoleTextView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		setupView(context, null, 0);
	}

	public ConsoleTextView(Context context, String msg, int cmdOrderNum) {
		super(context);
		setupView(context, msg, cmdOrderNum);
	}

	protected void setupView(Context context, String msg, int cmdOrderNum) {
		this.addTextChangedListener(new PrettyPrinter());
		Typeface mTypeface = Typeface.createFromAsset(context.getAssets(), TYPEFACE);

		this.setTypeface(mTypeface);
		this.setTextColor(Color.WHITE);
		this.setTextSize(TEXT_SIZE);
		this.setGravity(Gravity.BOTTOM);
		// TODO: Make a better ID system
		this.setId((int) System.currentTimeMillis());
		this.setText("hermit<" + cmdOrderNum + "> ");
		if (msg != null)
			this.append(msg);

		mCommandOrderNum = cmdOrderNum;
	}

	protected class PrettyPrinter implements TextWatcher {
		String lastText = null;

		@Override
		public void onTextChanged(CharSequence s, int start, int before,
				int count) {
			if (!s.toString().equals(lastText)) {
				lastText = s.toString();

				String res = "";
				//Make sure to sanitize string for HTML parsing
				String[] sentence = TextUtils.htmlEncode(s.toString()).split(" ");
				for (String word : sentence) {
					String color = null;
					if (word.equals(RED)) {
						color = "#CC060B";
					} else if (word.equals(GREEN)) {
						color = "#1DDA1C";
					} else if (word.equals(BLUE)) {
						color = "#0090D3";
					}

					if (color != null) {
						res += "<font color='" + color + "'>" +
								//"<a href='console://test' style='text-decoration:none;'>" +
								word + //"</a>
								"</font> ";
					} else {
						res += word + " ";
					}
				};
				ConsoleTextView.this.setText(Html.fromHtml(res));
			}
		}

		@Override
		public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

		@Override
		public void afterTextChanged(Editable s) {}

	}

}
