package edu.kufpg.armatus.util;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageButton;

public class TurboImageButton extends ImageButton {
	public static final int DEFAULT_INITIAL_INTERVAL = 400;
	public static final int DEFAULT_NORMAL_INTERVAL = 100;

	private OnClickListener mOnClickListener;
	private OnTurboListener mOnTurboListener;

	public TurboImageButton(Context context) {
		super(context);
		init();
	}

	public TurboImageButton(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public TurboImageButton(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	private void init() {
		mOnTurboListener = new OnTurboListener(DEFAULT_INITIAL_INTERVAL, DEFAULT_NORMAL_INTERVAL) {
			@Override
			public void onClick(View v) {
				if (mOnClickListener != null) {
					mOnClickListener.onClick(v);
				}
			}
		};
		setOnTouchListener(mOnTurboListener);
	}

	@Override
	public void setOnClickListener(OnClickListener l) {
		mOnClickListener = l;
	}

	public int getInitialInterval() {
		return mOnTurboListener.getInitialInterval();
	}

	public int getNormalInterval() {
		return mOnTurboListener.getNormalInterval();
	}

	public void setInitialInterval(int initialInterval) {
		mOnTurboListener.setInitialInterval(initialInterval);
	}

	public void setNormalInterval(int normalInterval) {
		mOnTurboListener.setNormalInterval(normalInterval);
	}

}
