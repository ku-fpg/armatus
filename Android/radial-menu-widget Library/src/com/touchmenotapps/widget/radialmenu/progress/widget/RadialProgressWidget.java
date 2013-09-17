package com.touchmenotapps.widget.radialmenu.progress.widget;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.Rect;
import android.graphics.RectF;
import android.graphics.Typeface;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;

public class RadialProgressWidget extends View {
	private RectF mRadialScoreRect;
	private int mCurrentValue = 15;
	private int mMaxValue = 100;
	private float mRadius = 0.0F;
	private int mDiameter = 200;
	private int mMaxSweepAngle = 360;
	private int[] mScoreColorRange;
	private Paint mRadialWidgetPaint = new Paint(1);
	private int mCurrentScoreColorPointer = 0;
	private int mBaseColor = Color.parseColor("#FF636363");
	private int mBorderColor = -3355444;
	private int mCenterTextColor = -1;
	private int mSecondaryTextColor = -1;
	private int mShadowColor = -16777216;
	private float mBorderStrokeThickness = 5.0F;
	private float mShadowRadius = 4.0F;
	private String mSecondaryText = null;
	private boolean mIsShowPercentText = true;
	private boolean mIsTouchEnabled = true;
	private float mCenterTextSize = 0.0F;
	private float mSecondaryTextSize = 0.0F;
	private int mReadingValuePer = 0;
	private int mAngle = 0;
	private int mMinChangeValue = 0;
	private int mMaxChangeValue = mMaxValue;
	private String mFontName = null;
	private OnRadialViewValueChanged mCallback;

	public RadialProgressWidget(Context context) {
		super(context);
		initView();
	}

	public RadialProgressWidget(Context context, AttributeSet attrs) {
		super(context, attrs);
		initView();
	}

	public RadialProgressWidget(Context context, AttributeSet attrs,
			int defStyle) {
		super(context, attrs, defStyle);
		initView();
	}

	private void initView() {
		Rect rect = new Rect(0, 0, mDiameter, mDiameter);
		mRadialScoreRect = new RectF(rect);
		mScoreColorRange = new int[] { -3407872, -48060, -30720,
				-13388315, -6697984 };
	}

	protected void onDraw(Canvas canvas) {
		super.onDraw(canvas);

		mRadialWidgetPaint.setStyle(Paint.Style.STROKE);
		mRadialWidgetPaint.setStrokeWidth(mBorderStrokeThickness
				* getResources().getDisplayMetrics().density);
		mRadialWidgetPaint.setColor(mBorderColor);
		canvas.drawCircle(getWidth() / 2, getHeight() / 2, mRadius,
				mRadialWidgetPaint);
		mRadialWidgetPaint.setStyle(Paint.Style.FILL);

		if (mCurrentValue <= mMaxValue) {
			double sweepAngle = mCurrentValue * mMaxSweepAngle
					/ mMaxValue;

			mReadingValuePer = (mCurrentValue * 100 / mMaxValue);
			for (int counter = 1; counter <= mScoreColorRange.length; counter++) {
				int colorPer = counter * 100 / mScoreColorRange.length;
				if (mReadingValuePer <= colorPer) {
					mCurrentScoreColorPointer = (counter - 1);
					break;
				}
			}

			mRadialWidgetPaint
					.setColor(mScoreColorRange[mCurrentScoreColorPointer]);
			canvas.drawArc(mRadialScoreRect, 270.0F, (float) sweepAngle,
					true, mRadialWidgetPaint);
			mRadialWidgetPaint.setShadowLayer(mShadowRadius / 2.0F
					* getResources().getDisplayMetrics().density, 0.0F, 0.0F,
					mShadowColor);
			canvas.drawArc(mRadialScoreRect, 270.0F, (float) sweepAngle,
					true, mRadialWidgetPaint);
			mRadialWidgetPaint.setShadowLayer(mShadowRadius, 0.0F,
					0.0F, 0);
		} else {
			Log.e(getClass().getName(),
					"Current value " + String.valueOf(mCurrentValue)
							+ " greater that maximum value "
							+ String.valueOf(mMaxValue));
		}
		mRadialWidgetPaint.setColor(mBaseColor);
		canvas.drawCircle(getWidth() / 2, getHeight() / 2,
				(float) (mRadius * 0.8D), mRadialWidgetPaint);
		mRadialWidgetPaint.setShadowLayer(mShadowRadius
				* getResources().getDisplayMetrics().density, 0.0F, 0.0F,
				mShadowColor);
		canvas.drawCircle(getWidth() / 2, getHeight() / 2,
				(float) (mRadius * 0.8D), mRadialWidgetPaint);
		mRadialWidgetPaint.setShadowLayer(mShadowRadius, 0.0F, 0.0F,
				0);

		mRadialWidgetPaint.setColor(mCenterTextColor);
		mRadialWidgetPaint.setTextSize(mCenterTextSize);
		if (mFontName != null)
			mRadialWidgetPaint.setTypeface(Typeface.createFromAsset(
					getContext().getAssets(), mFontName));
		float textWidth = 0.0F;

		if (mIsShowPercentText) {
			textWidth = mRadialWidgetPaint.measureText(String
					.valueOf(mReadingValuePer) + "%");
			canvas.drawText(String.valueOf(mReadingValuePer) + "%",
					getWidth() / 2 - textWidth / 2.0F, getHeight() / 2
							+ mRadius / 8.0F, mRadialWidgetPaint);
		} else {
			textWidth = mRadialWidgetPaint.measureText(String
					.valueOf(mCurrentValue));
			canvas.drawText(String.valueOf(mCurrentValue), getWidth() / 2
					- textWidth / 2.0F, getHeight() / 2 + mRadius / 8.0F,
					mRadialWidgetPaint);
		}

		if (mSecondaryText != null) {
			mRadialWidgetPaint.setColor(mSecondaryTextColor);
			textWidth = mRadialWidgetPaint
					.measureText(mSecondaryText);
			mRadialWidgetPaint.setTextSize(mSecondaryTextSize);
			canvas.drawText(mSecondaryText, getWidth() / 2 - textWidth
					/ 5.0F, getHeight() / 2 + mRadius / 3.0F,
					mRadialWidgetPaint);
		}
	}

	protected void onSizeChanged(int w, int h, int oldw, int oldh) {
		super.onSizeChanged(w, h, oldw, oldh);

		if (w > h) {
			mDiameter = h;
			mRadius = (mDiameter / 2 - (getPaddingTop() + getPaddingBottom()));
		} else {
			mDiameter = w;
			mRadius = (mDiameter / 2 - (getPaddingLeft() + getPaddingRight()));
		}

		int left = getWidth() / 2 - (int) mRadius + getPaddingLeft();
		int right = getWidth() / 2 + (int) mRadius - getPaddingRight();
		int top = getHeight() / 2 - (int) mRadius + getPaddingTop();
		int bottom = getHeight() / 2 + (int) mRadius - getPaddingBottom();
		Rect rect = new Rect(left, top, right, bottom);
		mRadialScoreRect = new RectF(rect);

		mCenterTextSize = (mRadius / 2.0F);
		mSecondaryTextSize = (mRadius / 5.0F);
	}

	public boolean onTouchEvent(MotionEvent event) {
		if (mIsTouchEnabled) {
			switch (event.getAction()) {
			case MotionEvent.ACTION_MOVE:
				mAngle = getAngleABC(new Point(getWidth() / 2, 0),
						new Point(getWidth() / 2, getHeight() / 2), new Point(
								(int) event.getX(), (int) event.getY()));
				Log.d("Test", "Angle " + mAngle);
				if ((mAngle * mMaxValue / mMaxSweepAngle > mMinChangeValue)
						&& (mAngle * mMaxValue / mMaxSweepAngle < mMaxChangeValue)) {
					setCurrentValue(mAngle * mMaxValue
							/ mMaxSweepAngle);
					if (mCallback != null)
						mCallback.onValueChanged(getCurrentValue());
					invalidate();
				}
				break;
			}
			return true;
		}
		return false;
	}

	private int getAngleABC(Point a, Point b, Point c) {
		Point ab = new Point(b.x - a.x, b.y - a.y);
		Point cb = new Point(b.x - c.x, b.y - c.y);
		float dot = ab.x * cb.x + ab.y * cb.y;
		float cross = ab.x * cb.y - ab.y * cb.x;
		float alpha = (float) Math.atan2(cross, dot);
		if ((int) Math.toDegrees(alpha) < 0) {
			return (int) Math.toDegrees(alpha) + 360;
		}
		return (int) Math.toDegrees(alpha);
	}

	public int getCurrentValue() {
		return mCurrentValue;
	}

	public void setOnRadialViewValueChanged(OnRadialViewValueChanged callback) {
		mCallback = callback;
	}

	public void setCurrentValue(int currentValue) {
		mCurrentValue = currentValue;
	}

	public int getMaxValue() {
		return mMaxValue;
	}

	public void setMaxValue(int maxValue) {
		mMaxValue = maxValue;
	}

	public int[] getScoreColorRange() {
		return mScoreColorRange;
	}

	public void setScoreColorRange(int[] scoreColorRange) {
		mScoreColorRange = scoreColorRange;
	}

	public int getBaseColor() {
		return mBaseColor;
	}

	public void setBaseColor(int baseColor) {
		mBaseColor = baseColor;
	}

	public int getBorderColor() {
		return mBorderColor;
	}

	public void setBorderColor(int borderColor) {
		mBorderColor = borderColor;
	}

	public int getCenterTextColor() {
		return mCenterTextColor;
	}

	public void setCenterTextColor(int centerTextColor) {
		mCenterTextColor = centerTextColor;
	}

	public int getSecondaryTextColor() {
		return mSecondaryTextColor;
	}

	public void setSecondaryTextColor(int secondaryTextColor) {
		mSecondaryTextColor = secondaryTextColor;
	}

	public int getShadowColor() {
		return mShadowColor;
	}

	public void setShadowColor(int shadowColor) {
		mShadowColor = shadowColor;
	}

	public float getBorderStrokeThickness() {
		return mBorderStrokeThickness;
	}

	public void setBorderStrokeThickness(float borderStrokeThickness) {
		mBorderStrokeThickness = borderStrokeThickness;
	}

	public float getShadowRadius() {
		return mShadowRadius;
	}

	public void setShadowRadius(float shadowRadius) {
		mShadowRadius = shadowRadius;
	}

	public String getSecondaryText() {
		return mSecondaryText;
	}

	public void setSecondaryText(String secondaryText) {
		mSecondaryText = secondaryText;
	}

	public boolean isShowPercentText() {
		return mIsShowPercentText;
	}

	public void setShowPercentText(boolean isShowPercentText) {
		mIsShowPercentText = isShowPercentText;
	}

	public float getCenterTextSize() {
		return mCenterTextSize;
	}

	public void setCenterTextSize(float centerTextSize) {
		mCenterTextSize = centerTextSize;
	}

	public float getSecondaryTextSize() {
		return mSecondaryTextSize;
	}

	public void setSecondaryTextSize(float secondaryTextSize) {
		mSecondaryTextSize = secondaryTextSize;
	}

	public boolean isTouchEnabled() {
		return mIsTouchEnabled;
	}

	public void setTouchEnabled(boolean isTouchEnabled) {
		mIsTouchEnabled = isTouchEnabled;
	}

	public int getMinChangeValue() {
		return mMinChangeValue;
	}

	public void setMinChangeValue(int minChangeValue) {
		mMinChangeValue = minChangeValue;
	}

	public int getMaxChangeValue() {
		return mMaxChangeValue;
	}

	public void setMaxChangeValue(int maxChangeValue) {
		mMaxChangeValue = maxChangeValue;
	}

	public void setFontName(String mFont) {
		mFontName = mFont;
	}

	public static abstract interface OnRadialViewValueChanged {
		public abstract void onValueChanged(int paramInt);
	}
}