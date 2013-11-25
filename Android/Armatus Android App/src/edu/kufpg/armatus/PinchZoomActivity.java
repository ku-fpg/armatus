package edu.kufpg.armatus;

import android.app.Activity;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.widget.TextView;
import edu.kufpg.armatus.gesture.OnPinchZoomListener;

public class PinchZoomActivity extends Activity {
	private final static int DEFAULT_SIZE = 15;

	private TextView mScalingView;
	private OnPinchZoomListener mZoomListener;
	private ScaleGestureDetector mScaleGestureDetector;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.textsize_pinchzoom);

		mZoomListener = new OnPinchZoomListener(this, DEFAULT_SIZE) {
			@Override
			public void onScaleEnd(ScaleGestureDetector detector) {
				mScalingView.setTextSize(getIntSize());
				super.onScaleEnd(detector);
			}
		};
		mScalingView = (TextView) findViewById(R.id.resize_me);
		mScalingView.setTextSize(DEFAULT_SIZE);
		mScaleGestureDetector = new ScaleGestureDetector(this, mZoomListener);
	}


	@Override
	public boolean onTouchEvent(MotionEvent event) {
		mScaleGestureDetector.onTouchEvent(event);
		return super.onTouchEvent(event);
	}

}
