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

	private boolean isShowPercentText = true;

	private boolean isTouchEnabled = true;

	private float mCenterTextSize = 0.0F;

	private float mSecondaryTextSize = 0.0F;

	private int readingValuePer = 0;

	private int angle = 0;

	private int mMinChangeValue = 0;

	private int mMaxChangeValue = this.mMaxValue;

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
		Rect rect = new Rect(0, 0, this.mDiameter, this.mDiameter);
		this.mRadialScoreRect = new RectF(rect);
		this.mScoreColorRange = new int[] { -3407872, -48060, -30720,
				-13388315, -6697984 };
	}

	protected void onDraw(Canvas canvas) {
		super.onDraw(canvas);

		this.mRadialWidgetPaint.setStyle(Paint.Style.STROKE);
		this.mRadialWidgetPaint.setStrokeWidth(this.mBorderStrokeThickness
				* getResources().getDisplayMetrics().density);
		this.mRadialWidgetPaint.setColor(this.mBorderColor);
		canvas.drawCircle(getWidth() / 2, getHeight() / 2, this.mRadius,
				this.mRadialWidgetPaint);
		this.mRadialWidgetPaint.setStyle(Paint.Style.FILL);

		if (this.mCurrentValue <= this.mMaxValue) {
			double sweepAngle = this.mCurrentValue * this.mMaxSweepAngle
					/ this.mMaxValue;

			this.readingValuePer = (this.mCurrentValue * 100 / this.mMaxValue);
			for (int counter = 1; counter <= this.mScoreColorRange.length; counter++) {
				int colorPer = counter * 100 / this.mScoreColorRange.length;
				if (this.readingValuePer <= colorPer) {
					this.mCurrentScoreColorPointer = (counter - 1);
					break;
				}
			}

			this.mRadialWidgetPaint
					.setColor(this.mScoreColorRange[this.mCurrentScoreColorPointer]);
			canvas.drawArc(this.mRadialScoreRect, 270.0F, (float) sweepAngle,
					true, this.mRadialWidgetPaint);
			this.mRadialWidgetPaint.setShadowLayer(this.mShadowRadius / 2.0F
					* getResources().getDisplayMetrics().density, 0.0F, 0.0F,
					this.mShadowColor);
			canvas.drawArc(this.mRadialScoreRect, 270.0F, (float) sweepAngle,
					true, this.mRadialWidgetPaint);
			this.mRadialWidgetPaint.setShadowLayer(this.mShadowRadius, 0.0F,
					0.0F, 0);
		} else {
			Log.e(getClass().getName(),
					"Current value " + String.valueOf(this.mCurrentValue)
							+ " greater that maximum value "
							+ String.valueOf(this.mMaxValue));
		}
		this.mRadialWidgetPaint.setColor(this.mBaseColor);
		canvas.drawCircle(getWidth() / 2, getHeight() / 2,
				(float) (this.mRadius * 0.8D), this.mRadialWidgetPaint);
		this.mRadialWidgetPaint.setShadowLayer(this.mShadowRadius
				* getResources().getDisplayMetrics().density, 0.0F, 0.0F,
				this.mShadowColor);
		canvas.drawCircle(getWidth() / 2, getHeight() / 2,
				(float) (this.mRadius * 0.8D), this.mRadialWidgetPaint);
		this.mRadialWidgetPaint.setShadowLayer(this.mShadowRadius, 0.0F, 0.0F,
				0);

		this.mRadialWidgetPaint.setColor(this.mCenterTextColor);
		this.mRadialWidgetPaint.setTextSize(this.mCenterTextSize);
		if (this.mFontName != null)
			this.mRadialWidgetPaint.setTypeface(Typeface.createFromAsset(
					getContext().getAssets(), this.mFontName));
		float textWidth = 0.0F;

		if (this.isShowPercentText) {
			textWidth = this.mRadialWidgetPaint.measureText(String
					.valueOf(this.readingValuePer) + "%");
			canvas.drawText(String.valueOf(this.readingValuePer) + "%",
					getWidth() / 2 - textWidth / 2.0F, getHeight() / 2
							+ this.mRadius / 8.0F, this.mRadialWidgetPaint);
		} else {
			textWidth = this.mRadialWidgetPaint.measureText(String
					.valueOf(this.mCurrentValue));
			canvas.drawText(String.valueOf(this.mCurrentValue), getWidth() / 2
					- textWidth / 2.0F, getHeight() / 2 + this.mRadius / 8.0F,
					this.mRadialWidgetPaint);
		}

		if (this.mSecondaryText != null) {
			this.mRadialWidgetPaint.setColor(this.mSecondaryTextColor);
			textWidth = this.mRadialWidgetPaint
					.measureText(this.mSecondaryText);
			this.mRadialWidgetPaint.setTextSize(this.mSecondaryTextSize);
			canvas.drawText(this.mSecondaryText, getWidth() / 2 - textWidth
					/ 5.0F, getHeight() / 2 + this.mRadius / 3.0F,
					this.mRadialWidgetPaint);
		}
	}

	protected void onSizeChanged(int w, int h, int oldw, int oldh) {
		super.onSizeChanged(w, h, oldw, oldh);

		if (w > h) {
			this.mDiameter = h;
			this.mRadius = (this.mDiameter / 2 - (getPaddingTop() + getPaddingBottom()));
		} else {
			this.mDiameter = w;
			this.mRadius = (this.mDiameter / 2 - (getPaddingLeft() + getPaddingRight()));
		}

		int left = getWidth() / 2 - (int) this.mRadius + getPaddingLeft();
		int right = getWidth() / 2 + (int) this.mRadius - getPaddingRight();
		int top = getHeight() / 2 - (int) this.mRadius + getPaddingTop();
		int bottom = getHeight() / 2 + (int) this.mRadius - getPaddingBottom();
		Rect rect = new Rect(left, top, right, bottom);
		this.mRadialScoreRect = new RectF(rect);

		this.mCenterTextSize = (this.mRadius / 2.0F);
		this.mSecondaryTextSize = (this.mRadius / 5.0F);
	}

	public boolean onTouchEvent(MotionEvent event) {
		if (this.isTouchEnabled) {
			switch (event.getAction()) {
			case 2:
				this.angle = getAngleABC(new Point(getWidth() / 2, 0),
						new Point(getWidth() / 2, getHeight() / 2), new Point(
								(int) event.getX(), (int) event.getY()));
				Log.d("Test", "Angle " + this.angle);
				if ((this.angle * this.mMaxValue / this.mMaxSweepAngle > this.mMinChangeValue)
						&& (this.angle * this.mMaxValue / this.mMaxSweepAngle < this.mMaxChangeValue)) {
					setCurrentValue(this.angle * this.mMaxValue
							/ this.mMaxSweepAngle);
					if (this.mCallback != null)
						this.mCallback.onValueChanged(getCurrentValue());
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
		return this.mCurrentValue;
	}

	public void setOnRadialViewValueChanged(OnRadialViewValueChanged callback) {
		this.mCallback = callback;
	}

	public void setCurrentValue(int mCurrentValue) {
		this.mCurrentValue = mCurrentValue;
	}

	public int getMaxValue() {
		return this.mMaxValue;
	}

	public void setMaxValue(int mMaxValue) {
		this.mMaxValue = mMaxValue;
	}

	public int[] getScoreColorRange() {
		return this.mScoreColorRange;
	}

	public void setScoreColorRange(int[] mScoreColorRange) {
		this.mScoreColorRange = mScoreColorRange;
	}

	public int getBaseColor() {
		return this.mBaseColor;
	}

	public void setBaseColor(int mBaseColor) {
		this.mBaseColor = mBaseColor;
	}

	public int getBorderColor() {
		return this.mBorderColor;
	}

	public void setBorderColor(int mBorderColor) {
		this.mBorderColor = mBorderColor;
	}

	public int getCenterTextColor() {
		return this.mCenterTextColor;
	}

	public void setCenterTextColor(int mCenterTextColor) {
		this.mCenterTextColor = mCenterTextColor;
	}

	public int getSecondaryTextColor() {
		return this.mSecondaryTextColor;
	}

	public void setSecondaryTextColor(int mSecondaryTextColor) {
		this.mSecondaryTextColor = mSecondaryTextColor;
	}

	public int getShadowColor() {
		return this.mShadowColor;
	}

	public void setShadowColor(int mShadowColor) {
		this.mShadowColor = mShadowColor;
	}

	public float getBorderStrokeThickness() {
		return this.mBorderStrokeThickness;
	}

	public void setBorderStrokeThickness(float mBorderStrokeThickness) {
		this.mBorderStrokeThickness = mBorderStrokeThickness;
	}

	public float getShadowRadius() {
		return this.mShadowRadius;
	}

	public void setShadowRadius(float mShadowRadius) {
		this.mShadowRadius = mShadowRadius;
	}

	public String getSecondaryText() {
		return this.mSecondaryText;
	}

	public void setSecondaryText(String mSecondaryText) {
		this.mSecondaryText = mSecondaryText;
	}

	public boolean isShowPercentText() {
		return this.isShowPercentText;
	}

	public void setShowPercentText(boolean isShowPercentText) {
		this.isShowPercentText = isShowPercentText;
	}

	public float getCenterTextSize() {
		return this.mCenterTextSize;
	}

	public void setCenterTextSize(float mCenterTextSize) {
		this.mCenterTextSize = mCenterTextSize;
	}

	public float getSecondaryTextSize() {
		return this.mSecondaryTextSize;
	}

	public void setSecondaryTextSize(float mSecondaryTextSize) {
		this.mSecondaryTextSize = mSecondaryTextSize;
	}

	public boolean isTouchEnabled() {
		return this.isTouchEnabled;
	}

	public void setTouchEnabled(boolean isTouchEnabled) {
		this.isTouchEnabled = isTouchEnabled;
	}

	public int getMinChangeValue() {
		return this.mMinChangeValue;
	}

	public void setMinChangeValue(int mMinChangeValue) {
		this.mMinChangeValue = mMinChangeValue;
	}

	public int getMaxChangeValue() {
		return this.mMaxChangeValue;
	}

	public void setMaxChangeValue(int mMaxChangeValue) {
		this.mMaxChangeValue = mMaxChangeValue;
	}

	public void setFontName(String mFont) {
		this.mFontName = mFont;
	}

	public static abstract interface OnRadialViewValueChanged {
		public abstract void onValueChanged(int paramInt);
	}
}