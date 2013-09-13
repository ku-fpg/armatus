package com.touchmenotapps.radialdemo.widgets;

import android.content.Context;
import android.graphics.Rect;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.widget.TextView;

public class AppTextView extends TextView {
	
	public AppTextView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		setTypeface(Typeface.MONOSPACE);
		setHorizontalFadingEdgeEnabled(false);
	}

	public AppTextView(Context context, AttributeSet attrs) {
		super(context, attrs);
		setTypeface(Typeface.MONOSPACE);
		setHorizontalFadingEdgeEnabled(false);
	}

	public AppTextView(Context context) {
		super(context);
		setTypeface(Typeface.MONOSPACE);
		setHorizontalFadingEdgeEnabled(false);
	}

	@Override
	protected void onFocusChanged(boolean focused, int direction,
			Rect previouslyFocusedRect) {
		if (focused)
			super.onFocusChanged(focused, direction, previouslyFocusedRect);
	}

	@Override
	public void onWindowFocusChanged(boolean focused) {
		if (focused)
			super.onWindowFocusChanged(focused);
	}

	@Override
	public boolean isFocused() {
		return true;
	}
	
}
