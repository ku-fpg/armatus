package edu.kufpg.armatus.gesture;

import edu.kufpg.armatus.R;
import android.app.Activity;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.ScaleGestureDetector;
import android.view.View;
import android.view.ViewGroup;
import android.view.ScaleGestureDetector.SimpleOnScaleGestureListener;
import android.widget.PopupWindow;
import android.widget.TextView;

public class OnPinchZoomListener extends SimpleOnScaleGestureListener {
	public final static int DEFAULT_SIZE = 20;
	public final static int DEFAULT_MIN_SIZE = 15;
	public final static int DEFAULT_MAX_SIZE = 60;

	private Activity mActivity;
	private PopupWindow mPopup;
	private View mPopupView;
	private TextView mPopupTextView;

	private int mIntSize;
	private float mFloatSize;
	private int mMinSize;
	private int mMaxSize;

	public OnPinchZoomListener(Activity activity) {
		this(activity, DEFAULT_SIZE, DEFAULT_MIN_SIZE, DEFAULT_MAX_SIZE);
	}

	public OnPinchZoomListener(Activity activity, int size) {
		this(activity, size, DEFAULT_MIN_SIZE, DEFAULT_MAX_SIZE);
	}

	public OnPinchZoomListener(Activity activity, int size, int minSize, int maxSize) {
		mActivity = activity;

		if (size < minSize || size > maxSize) {
			throw new IllegalArgumentException("size (" + size + ") is not between minSize ("
					+ minSize + ") and maxSize (" + maxSize + ")");
		}

		mIntSize = size;
		mFloatSize = size;
		mMinSize = minSize;
		mMaxSize = maxSize;

		LayoutInflater inf = LayoutInflater.from(activity);
		mPopupView = inf.inflate(R.layout.popup,
				(ViewGroup) activity.findViewById(R.id.popup_layout));
		mPopupView.measure(View.MeasureSpec.UNSPECIFIED, View.MeasureSpec.UNSPECIFIED);
		mPopupTextView = (TextView) mPopupView.findViewById(R.id.popup_textview);
		refreshPopup(size);
		mPopup = new PopupWindow(mPopupView, ViewGroup.LayoutParams.WRAP_CONTENT,
				ViewGroup.LayoutParams.WRAP_CONTENT, false);
	}

	@Override
	public boolean onScale(ScaleGestureDetector detector) {
		float size = mFloatSize;
		float factor = detector.getScaleFactor();
		float product = size*factor;
		product = Math.max(mMinSize, Math.min(product, mMaxSize));
		refreshPopup((int) product);
		mFloatSize = product;
		return true;
	}

	@Override
	public boolean onScaleBegin(ScaleGestureDetector detector) {
		mPopup.showAtLocation(mPopupView, Gravity.CENTER, 0, 0);
		return true;
	}

	@Override
	public void onScaleEnd(ScaleGestureDetector detector) {
		mPopup.dismiss();
	}

	public int getIntSize() {
		return mIntSize;
	}

	public float getFloatSize() {
		return mFloatSize;
	}
	
	public void setSize(int newSize) {
		mFloatSize = newSize;
		refreshPopup(newSize);
	}

	private void refreshPopup(int newFontSize) {
		mIntSize = newFontSize;
		String label = mActivity.getResources().getString(R.string.popup_text);
		mPopupTextView.setText(label + " " + newFontSize);
	}

}
