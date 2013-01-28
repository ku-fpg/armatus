package com.kufpg.androidhermit.util;

import java.util.ArrayList;

import android.content.Context;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;
import android.widget.RelativeLayout;
import android.widget.TextView;

public class CodeLayout extends RelativeLayout {

	private final static float STEP = 200;
	private final static int DEFAULT_FONT_SIZE = ConsoleTextView.DEFAULT_FONT_SIZE;
	private final static int MAX_FONT_SIZE = ConsoleTextView.MAX_FONT_SIZE;
	private final static int MIN_FONT_SIZE = ConsoleTextView.MIN_FONT_SIZE;

	private float mRatio = 1.0f;
	private float mBaseRatio;
	private int mBaseDist;

	public CodeLayout(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		setupCodeLayout();
	}

	public CodeLayout(Context context, AttributeSet attrs) {
		super(context, attrs);
		setupCodeLayout();
	}

	public CodeLayout(Context context) {
		super(context);
		setupCodeLayout();
	}

	private void setupCodeLayout() {

	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {
		if (event.getPointerCount() == 2) {
			int action = event.getAction();
			int pureaction = action & MotionEvent.ACTION_MASK;
			if (pureaction == MotionEvent.ACTION_POINTER_DOWN) {
				mBaseDist = getDistance(event);
				mBaseRatio = mRatio;
			} else {
				float delta = (getDistance(event) - mBaseDist) / STEP;
				float multi = (float)Math.pow(2, delta);
				mRatio = Math.min(1024.0f, Math.max(0.1f, mBaseRatio * multi));
				int newFontSize = (int) mRatio + DEFAULT_FONT_SIZE;
				if (newFontSize > MAX_FONT_SIZE) {
					newFontSize = MAX_FONT_SIZE;
				} else if (newFontSize < MIN_FONT_SIZE) {
					newFontSize = MIN_FONT_SIZE;
				}
//				for (TextView tv : mTextViews) {
//					tv.setTextSize(newFontSize);
//				}
			}
		}
		return true;
	}

	private int getDistance(MotionEvent event) {
		int dx = (int)(event.getX(0) - event.getX(1));
		int dy = (int)(event.getY(0) - event.getY(1));
		return (int)(Math.sqrt(dx * dx + dy * dy));
	}

}
