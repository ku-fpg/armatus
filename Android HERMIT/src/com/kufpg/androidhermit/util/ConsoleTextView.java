package com.kufpg.androidhermit.util;

import android.content.Context;
import android.content.res.ColorStateList;
import android.graphics.Color;
import android.graphics.Typeface;
import android.provider.CalendarContract.Colors;
import android.util.AttributeSet;
import android.view.Gravity;
import android.widget.TextView;

public class ConsoleTextView extends TextView {

	private final float TEXT_SIZE = 15;

	public ConsoleTextView(Context context, String msg) {
		super(context);
		setupView(msg);
	}

	public ConsoleTextView(Context context, AttributeSet attrs, String msg) {
		super(context, attrs);
		setupView(msg);
	}

	public ConsoleTextView(Context context, AttributeSet attrs, int defStyle, String msg) {
		super(context, attrs, defStyle);
		setupView(msg);
	}

	private void setupView(String msg) {
		this.setTypeface(Typeface.MONOSPACE);
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



}
