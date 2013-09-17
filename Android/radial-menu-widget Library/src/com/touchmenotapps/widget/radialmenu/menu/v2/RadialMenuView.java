package com.touchmenotapps.widget.radialmenu.menu.v2;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.RectF;
import android.view.MotionEvent;
import android.view.View;

import java.util.ArrayList;
import java.util.List;

public class RadialMenuView extends View {
	private List<RadialMenuItem> mRadialMenuContent = new ArrayList<RadialMenuItem>(0);
	private boolean mAlt;
	private float mWidth = -1.0F;
	private float mHeight = -1.0F;
	private float mThickness;
	private float mRadius;
	private int mSelected = -1;
	private int mLastE = -1;
	private float[] mEndTouch;
	private Paint mBgPaint = new Paint(1);
	private Paint mTextPaint = new Paint(1);
	private Paint mSelectedPaint = new Paint(1);
	private Paint mBorderPaint = new Paint(1);

	private RectF mRectF = new RectF();
	private Path mArc = new Path();

	public RadialMenuView(Context context) {
		super(context);
	}

	public RadialMenuView(Context context, RadialMenuRenderer renderer) {
		super(context);
		mRadialMenuContent = renderer.getRadialMenuContent();
		mAlt = renderer.isAlt();
		mThickness = renderer.getMenuThickness();
		mRadius = renderer.getRadius();
		setVisibility(8);
		initSetPaint(renderer);
	}

	private void initSetPaint(RadialMenuRenderer renderer) {
		mBgPaint.setColor(renderer.getMenuBackgroundColor());
		mBgPaint.setStrokeWidth(renderer.getMenuThickness());
		mBgPaint.setStyle(Paint.Style.STROKE);

		mSelectedPaint.setColor(renderer.getMenuSelectedColor());
		mSelectedPaint.setStrokeWidth(renderer.getMenuThickness());
		mSelectedPaint.setStyle(Paint.Style.STROKE);

		mBorderPaint.setColor(renderer.getMenuBorderColor());
		mBorderPaint.setStrokeWidth(renderer.getMenuThickness());
		mBorderPaint.setStyle(Paint.Style.STROKE);

		mTextPaint.setColor(renderer.getMenuTextColor());
		mTextPaint.setTextSize(renderer.getMenuThickness() / 2.0F);
	}

	public void setLoc(float x, float y) {
		if (x < mRadius + mThickness / 2.0F)
			x = mRadius + mThickness / 2.0F;
		if (y < mRadius + mThickness / 2.0F) {
			y = mRadius + mThickness / 2.0F;
		}
		if (y > getHeight() - (mRadius + mThickness / 2.0F))
			y = getHeight() - (mRadius + mThickness / 2.0F);
		if (x > getWidth() - (mRadius + mThickness / 2.0F)) {
			x = getWidth() - (mRadius + mThickness / 2.0F);
		}
		mWidth = x;
		mHeight = y;
	}

	public void onDraw(Canvas canvas) {
		setLoc(mWidth, mHeight);
		mRectF.setEmpty();
		mRectF.set(mWidth - mRadius, mHeight - mRadius,
				mWidth + mRadius, mHeight + mRadius);
		int tot = mRadialMenuContent.size();

		mBorderPaint.setStrokeWidth(mThickness);

		for (int counter = 0; counter < tot; counter++) {
			if (!((RadialMenuItem) mRadialMenuContent.get(counter))
					.equals("HOLLOW")) {
				if (mAlt)
					canvas.drawArc(mRectF, 360 / tot * counter - 90 - 360 / tot
							/ 2, 360 / tot, false,
							mSelected == counter ? mSelectedPaint
									: mBgPaint);
				else {
					canvas.drawArc(mRectF, 360 / tot * counter - 90, 360 / tot,
							false,
							mSelected == counter ? mSelectedPaint
									: mBgPaint);
				}
			}
		}
		for (int counter = 0; counter < tot; counter++) {
			if (!((RadialMenuItem) mRadialMenuContent.get(counter))
					.equals("HOLLOW")) {
				mArc.reset();
				if (mAlt) {
					mArc.addArc(mRectF, 360 / tot * counter - 90 - 360 / tot / 2
							+ 10.0F, 360 / tot - 10.0F);
					canvas.drawTextOnPath(
							((RadialMenuItem) mRadialMenuContent
									.get(counter)).getMenuName(), mArc, 0.0F,
									mThickness / 8.0F, mTextPaint);
				} else {
					mArc.addArc(mRectF, 360 / tot * counter - 90 + 10.0F,
							360 / tot - 10.0F);
					canvas.drawTextOnPath(
							((RadialMenuItem) mRadialMenuContent
									.get(counter)).getMenuName(), mArc, 0.0F,
									-mThickness / 8.0F, mTextPaint);
				}
			}

		}

		if (tot > 1) {
			for (int counter = 0; counter < tot; counter++) {
				if (!((RadialMenuItem) mRadialMenuContent.get(counter))
						.equals("HOLLOW")) {
					if (mAlt) {
						canvas.drawArc(mRectF, 360 / tot * counter - 91 - 360
								/ tot / 2, 2.0F, false, mBorderPaint);
						canvas.drawArc(mRectF, 360 / tot * (counter + 1) - 91
								- 360 / tot / 2, 2.0F, false, mBorderPaint);
					} else {
						canvas.drawArc(mRectF, 360 / tot * counter - 91, 2.0F,
								false, mBorderPaint);
						canvas.drawArc(mRectF, 360 / tot * (counter + 1) - 91,
								2.0F, false, mBorderPaint);
					}
				}
			}
		}
		mBorderPaint.setStrokeWidth(2.0F);
		mRectF.set(mWidth - mRadius - mThickness / 2.0F,
				mHeight - mRadius - mThickness / 2.0F,
				mWidth + mRadius + mThickness / 2.0F,
				mHeight + mRadius + mThickness / 2.0F);

		for (int counter = 0; counter < tot; counter++) {
			if (!((RadialMenuItem) mRadialMenuContent.get(counter))
					.equals("HOLLOW")) {
				if (mAlt)
					canvas.drawArc(mRectF, 360 / tot * counter - 91 - 360 / tot
							/ 2, 360 / tot + 2.0F, false, mBorderPaint);
				else {
					canvas.drawArc(mRectF, 360 / tot * counter - 91,
							360 / tot + 2.0F, false, mBorderPaint);
				}
			}
		}
		mRectF.set(mWidth - mRadius + mThickness / 2.0F,
				mHeight - mRadius + mThickness / 2.0F,
				mWidth + mRadius - mThickness / 2.0F,
				mHeight + mRadius - mThickness / 2.0F);

		for (int counter = 0; counter < tot; counter++)
			if (!((RadialMenuItem) mRadialMenuContent.get(counter))
					.equals("HOLLOW"))
				if (mAlt)
					canvas.drawArc(mRectF, 360 / tot * counter - 91 - 360 / tot
							/ 2, 360 / tot + 1.0F, false, mBorderPaint);
				else
					canvas.drawArc(mRectF, 360 / tot * counter - 91,
							360 / tot + 1.0F, false, mBorderPaint);
	}

	private boolean handleEvent(int e) {
		if (e == mRadialMenuContent.size()) {
			e = 0;
		} else if (e == -1) {
			mSelected = -1;
			return false;
		}
		if (((RadialMenuItem) mRadialMenuContent.get(e)).getMenuName()
				.equals("HOLLOW")) {
			mSelected = -1;
			invalidate();
			return false;
		}
		((RadialMenuItem) mRadialMenuContent.get(e))
		.getOnClickListener().onClick(
				((RadialMenuItem) mRadialMenuContent.get(e))
				.getMenuID());
		mSelected = -1;
		invalidate();
		return true;
	}

	private void preEvent(int e) {
		if (e == mRadialMenuContent.size())
			e = 0;
		else if (mLastE == e)
			return;
		mLastE = e;
		if (e == -1) {
			mSelected = -1;
			invalidate();
			return;
		}
		if (((RadialMenuItem) mRadialMenuContent.get(e)).getMenuName()
				.equals("HOLLOW")) {
			mSelected = -1;
			invalidate();
			return;
		}
		mSelected = e;
		invalidate();
	}

	public boolean gestureHandler(MotionEvent event) {
		switch (event.getAction()) {
		case MotionEvent.ACTION_DOWN:
			mWidth = event.getX();
			mHeight = event.getY();
			setVisibility(VISIBLE);
			invalidate();
			return false;
		case MotionEvent.ACTION_UP:
			mEndTouch = new float[] { event.getX(), event.getY() };
			if (distance(mWidth, mHeight,
					mEndTouch[0], mEndTouch[1]) > mRadius
					- mThickness / 2.0F) {
				setVisibility(GONE);
				return handleEvent((int) angle(
						mWidth, mHeight, mEndTouch[0],
						mEndTouch[1], mAlt,
						mRadialMenuContent.size()));
			}
			setVisibility(GONE);
			return true;
		case MotionEvent.ACTION_MOVE:
			mEndTouch = new float[] { event.getX(), event.getY() };
			if (distance(mWidth, mHeight,
					mEndTouch[0], mEndTouch[1]) > mRadius
					- mThickness / 2.0F)
				preEvent((int) angle(mWidth,
						mHeight, mEndTouch[0], mEndTouch[1],
						mAlt, mRadialMenuContent.size()));
			else {
				preEvent(-1);
			}
			return false;
		}
		return true;
	}
	
	public void setMenuContent(List<RadialMenuItem> menuContent) {
		mRadialMenuContent = menuContent;
	}

	private static float distance(float width, float height, float x2, float y2) {
		double dx = width - x2;
		double dy = height - y2;
		float dist = (float) Math.sqrt(dx * dx + dy * dy);
		return dist;
	}

	private static float angle(float mWidth, float height, float x2, float y2,
			boolean alt, int items) {
		double dx = x2 - mWidth;
		double dy = y2 - height;
		float angle = (float) (Math.atan2(dy, dx) * 180.0D / 3.141592653589793D)
				+ 90.0F + (alt ? 360 / items / 2 : 0);
		if (angle < 0.0F)
			return (angle + 360.0F) / (360 / items);
		return angle / (360 / items);
	}
}