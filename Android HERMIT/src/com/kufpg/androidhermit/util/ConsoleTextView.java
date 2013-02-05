package com.kufpg.androidhermit.util;

import java.io.Serializable;
import java.util.ArrayList;

import com.kufpg.androidhermit.ConsoleActivity;
import com.kufpg.androidhermit.R;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.text.Editable;
import android.text.Html;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.MotionEvent;
import android.widget.TextView;

public class ConsoleTextView extends TextView implements Serializable {

	private static final long serialVersionUID = 492620301229198361L;
	private static final int PADDING = ConsoleActivity.PADDING;

	private int mCommandOrderNum;
	private ArrayList<String> mKeywords = new ArrayList<String>();

	public ConsoleTextView(Context context) {
		super(context);
	}

	public ConsoleTextView(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public ConsoleTextView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public ConsoleTextView(ConsoleActivity console, String msg, int cmdOrderNum) {
		super(console);
		setupView(console, msg, cmdOrderNum);
	}

	protected void setupView(ConsoleActivity console, String msg, int cmdOrderNum) {
		mCommandOrderNum = cmdOrderNum;
		Typeface typeface = Typeface.createFromAsset(getResources().getAssets(),
				ConsoleActivity.TYPEFACE);

		addTextChangedListener(new PrettyPrinter());
		setGravity(Gravity.BOTTOM);
		// TODO: Make a better ID system
		setId((int) System.currentTimeMillis());
		setPaddingRelative(PADDING, PADDING, PADDING, 0);
		setTextColor(Color.WHITE);
		setTextSize(ConsoleActivity.DEFAULT_FONT_SIZE);
		setTypeface(typeface);

		setText("hermit<" + cmdOrderNum + "> ");		
		if (msg != null) {
			append(msg);
			String[] inputArr = msg.split(ConsoleActivity.WHITESPACE);
			for(String word : inputArr) {
				if(CommandDispatcher.isKeyword(word)) {
					mKeywords.add(word);
				}
			}
		}
	}

	public int getCommandOrderNum() {
		return mCommandOrderNum;
	}

	public ArrayList<String> getKeywords() {
		return mKeywords;
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {
		if(event.getAction() == MotionEvent.ACTION_DOWN) {
			setBackground(getResources().getDrawable(R.drawable.console_text_border));
		} else if(event.getAction() == MotionEvent.ACTION_UP
				|| event.getAction() == MotionEvent.ACTION_OUTSIDE) {
			setBackgroundColor(getResources().getColor(android.R.color.transparent));
		}
		return super.onTouchEvent(event);	
	}

	public class PrettyPrinter implements TextWatcher {
		//		public static final String RED = "red";
		//		public static final String BLUE = "blue";
		//		public static final String GREEN = "green";

		public static final String RED = "#CC060B";
		public static final String GREEN = "#1DDA1C";
		public static final String BLUE = "#0090D3";
		private String lastText = null;

		@Override
		public void onTextChanged(CharSequence s, int start,
				int before, int count) {
			if (!s.toString().equals(lastText)) {
				lastText = s.toString();

				String res = "";
				//Make sure to sanitize string for HTML parsing
				String[] sentence = TextUtils.htmlEncode(s.toString())
						.split(ConsoleActivity.WHITESPACE);
				for (String word : sentence) {
					String color = null;
					if (CommandDispatcher.isKeyword(word)) {
						color = CommandDispatcher.getKeyword(word).getColor();
						res += "<font color='" + color + "'>" +
								//"<a href='console://test' style='text-decoration:none;'>" +
								//In the future, the above line could be used to hyperlink commands
								word + //"</a>
								"</font> ";
					} else {
						res += word + " ";
					}
				};
				res.trim();
				ConsoleTextView.this.setText(Html.fromHtml(res));
			}
		}

		@Override
		public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

		@Override
		public void afterTextChanged(Editable s) {}
	}

}