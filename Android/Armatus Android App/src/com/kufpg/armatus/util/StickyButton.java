package com.kufpg.armatus.util;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import android.content.Context;
import android.util.AttributeSet;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;

public class StickyButton extends Button implements OnClickListener {
	private OnStickListener mOnStickListener;
	private boolean mIsStuck = false;
	private final ReentrantLock mLock = new ReentrantLock(true);
	private final Condition mLockInEffect = mLock.newCondition();

	public StickyButton(Context context) {
		super(context);
		init();
	}

	public StickyButton(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public StickyButton(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}
	
	public void init() {
		setOnClickListener(this);
	}
	
	@Override
	public final void onClick(View v) {
		Log.d("TESTTESt", "whee");
		mLock.lock();
		try {
			while (mIsStuck) {
				try {
					mLockInEffect.await();
				} catch (InterruptedException e) {
					e.printStackTrace();
				}
			}
			mIsStuck = true;
			setEnabled(false);
			if (mOnStickListener != null) {
				mOnStickListener.onStick(v);
			}
		} finally {
			mLock.unlock();
		}
	}
	
	public void setOnStickListener(OnStickListener l) {
		mOnStickListener = l;
	}

	public void unstick() {
		mLock.lock();
		try {
			if (mIsStuck) {
				mLockInEffect.signal();
				mIsStuck = false;
				setEnabled(true);
			}
		} finally {
			mLock.unlock();
		}
	}
	
	public interface OnStickListener {
		void onStick(View v);
	};

}
