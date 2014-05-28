package edu.kufpg.armatus.util;

import android.os.Handler;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;

/**
 * A class, that can be used as a TouchListener on any view (e.g. a Button).
 * It cyclically runs a clickListener, emulating keyboard-like behaviour. First
 * click is fired immediately, next after initialInterval, and subsequent after
 * normalInterval.
 *
 * <p>Interval is scheduled after the onClick completes, so it has to run fast.
 * If it runs slow, it does not generate skipped onClicks.
 * 
 * @author <a href="http://stackoverflow.com/users/952135/oliv">Oliv</a> of StackOverflow
 * (see <a href="http://stackoverflow.com/a/12795551">http://stackoverflow.com/a/12795551</a>)
 */
public abstract class OnTurboListener implements OnTouchListener {

	private Handler mHandler = new Handler();
	private int mInitialInterval;
	private int mNormalInterval;
	private boolean mEnabled = true;
	private View mDownView;
	private Runnable mHandlerRunnable = new Runnable() {
		@Override
		public void run() {
			synchronized (this) {
				if (mEnabled) {
					mHandler.postDelayed(this, mNormalInterval);
					onClick(mDownView);
				}
			}
		}
	};

	/**
	 * @param initialInterval The interval after first click event
	 * @param normalInterval The interval after second and subsequent click 
	 *       events
	 * @param clickListener The OnClickListener, that will be called
	 *       periodically
	 */
	public OnTurboListener(int initialInterval, int normalInterval) {
		if (initialInterval < 0 || normalInterval < 0) {
			throw new IllegalArgumentException("Negative interval");
		}

		mInitialInterval = initialInterval;
		mNormalInterval = normalInterval;
	}

	public abstract void onClick(View v);

	@Override
	public boolean onTouch(View view, MotionEvent motionEvent) {
		switch (motionEvent.getAction()) {
		case MotionEvent.ACTION_DOWN:
			mHandler.removeCallbacks(mHandlerRunnable);
			synchronized (this) {
				mHandler.postDelayed(mHandlerRunnable, mInitialInterval);
				mDownView = view;
				onClick(view);
			}
			return false;
		case MotionEvent.ACTION_UP:
			mHandler.removeCallbacks(mHandlerRunnable);
			mDownView = null;
			return false;
		default:
			return false;
		}
	}

	public synchronized void disableTurbo() {
		mEnabled = false;
	}

	public synchronized void enableTurbo() {
		mEnabled = true;
	}

	public synchronized int getInitialInterval() {
		return mInitialInterval;
	}

	public synchronized int getNormalInterval() {
		return mNormalInterval;
	}

	public synchronized void setInitialInterval(int initialInterval) {
		mInitialInterval = initialInterval;
	}

	public synchronized void setNormalInterval(int normalInterval) {
		mNormalInterval = normalInterval;
	}

}