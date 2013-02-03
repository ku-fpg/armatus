package com.kufpg.androidhermit;

import com.kufpg.androidhermit.util.ActivitySwipeDetector;

import android.os.Bundle;
import android.app.Activity;
import android.content.Context;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.view.ScaleGestureDetector.SimpleOnScaleGestureListener;
import android.view.View;
import android.view.ViewGroup;
import android.widget.PopupWindow;
import android.widget.RelativeLayout;
import android.widget.TextView;

public class TextSizePinchZoomActivity extends Activity {

	private final static int DEFAULT_FONT_SIZE = ConsoleActivity.DEFAULT_FONT_SIZE;
	private final static int MAX_FONT_SIZE = ConsoleActivity.MAX_FONT_SIZE;
	private final static int MIN_FONT_SIZE = ConsoleActivity.MIN_FONT_SIZE;
	
	private TextView mScalingView;
	private ScaleGestureDetector mScaleGestureDetector;
	private PopupWindow mPopup;
	private RelativeLayout mLayout;
	private View mPopupView;
	private TextView mPopupTextView;
	private int mIntFontSize = DEFAULT_FONT_SIZE;
	private float mFloatFontSize = DEFAULT_FONT_SIZE;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.textsize_pinchzoom);
		mScalingView = (TextView) findViewById(R.id.resize_me);
		mScalingView.setTextSize(DEFAULT_FONT_SIZE);
		mScaleGestureDetector = new ScaleGestureDetector(this, new OnConsoleScaleGestureListener());
		
		//Fling test
		ActivitySwipeDetector activitySwipeDetector = new ActivitySwipeDetector(this);
		mLayout = (RelativeLayout) this.findViewById(R.id.fling_layout);
		mLayout.setOnTouchListener(activitySwipeDetector);
		
		LayoutInflater inflater = (LayoutInflater)
				getSystemService(Context.LAYOUT_INFLATER_SERVICE);
		mPopupView = inflater.inflate(R.layout.popup,
                (ViewGroup) findViewById(R.id.popup_layout));
		mPopupView.measure(View.MeasureSpec.UNSPECIFIED, View.MeasureSpec.UNSPECIFIED);
		mPopupTextView = (TextView) mPopupView.findViewById(R.id.popup_textview);
		refreshPopup(DEFAULT_FONT_SIZE);
		mPopup = new PopupWindow(mPopupView, ViewGroup.LayoutParams.WRAP_CONTENT,
				ViewGroup.LayoutParams.WRAP_CONTENT, false);
	}


	@Override
	public boolean onTouchEvent(MotionEvent event) {
		if (mScaleGestureDetector.onTouchEvent(event)) {
			if ((event.getAction() == MotionEvent.ACTION_DOWN || event.getAction() == MotionEvent.ACTION_MOVE)
					&& mScaleGestureDetector.isInProgress()) {
				mPopup.showAtLocation(mPopupView, Gravity.CENTER, 0, 0);
				return true;
			} else if (event.getAction() == MotionEvent.ACTION_UP) {
				mScalingView.setTextSize(mIntFontSize);
				mPopup.dismiss();
				return false;
			}
		}
		return super.onTouchEvent(event);
	}

	public class OnConsoleScaleGestureListener extends SimpleOnScaleGestureListener {
		@Override
		public boolean onScale(ScaleGestureDetector detector) {
			//float size = scaleGesture.getTextSize();
			float size = mFloatFontSize;
			float factor = detector.getScaleFactor();
			float product = size*factor;
			product = Math.max(MIN_FONT_SIZE, Math.min(product, MAX_FONT_SIZE));
			//scaleGesture.setTextSize(product);
			//size = scaleGesture.getTextSize();
			refreshPopup((int) product);
			mFloatFontSize = product;
			return true;
		}
	}
	
	private void refreshPopup(int newFontSize) {
		mIntFontSize = newFontSize;
		String label = getResources().getString(R.string.popup_text);
		mPopupTextView.setText(label + " " + newFontSize);
	}
}