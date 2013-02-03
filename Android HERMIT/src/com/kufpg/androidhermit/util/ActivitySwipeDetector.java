package com.kufpg.androidhermit.util;

import android.app.Activity;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.widget.Toast;

public class ActivitySwipeDetector implements OnTouchListener {

	static final String logTag = "ActivitySwipeDetector";
	private Activity activity;
	static final int MIN_DISTANCE = 100;
	private float downX, downY, upX, upY;

	public ActivitySwipeDetector(Activity activity){
		this.activity = activity;
	}

	public void onRightToLeftSwipe(){
		Log.i(logTag, "RightToLeftSwipe!");
		Toast.makeText(activity, "Leftward swipe!", Toast.LENGTH_SHORT).show();
	}

	public void onLeftToRightSwipe(){
		Log.i(logTag, "LeftToRightSwipe!");
		Toast.makeText(activity, "Rightward swipe!", Toast.LENGTH_SHORT).show();
	}

	public boolean onTouch(View v, MotionEvent event) {
		switch(event.getAction()) {
		case MotionEvent.ACTION_DOWN: {
			downX = event.getX();
			downY = event.getY();
			return true;
		}
		case MotionEvent.ACTION_UP: {
			upX = event.getX();
			upY = event.getY();

			float deltaX = downX - upX;

			// swipe horizontal?
			if(Math.abs(deltaX) > MIN_DISTANCE){
				// left or right
				if(deltaX < 0) {
					this.onLeftToRightSwipe(); return true;
				} else if(deltaX > 0) {
					this.onRightToLeftSwipe();
					return true;
				}
			} else {
				Log.i(logTag, "Swipe was only " + Math.abs(deltaX) + " long, need at least " + MIN_DISTANCE);
				return false; // We don't consume the event
			}

			return true;
		}
		}
		return false;
	}
}