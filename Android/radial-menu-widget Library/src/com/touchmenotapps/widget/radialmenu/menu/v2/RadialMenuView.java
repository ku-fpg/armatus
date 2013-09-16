package com.touchmenotapps.widget.radialmenu.menu.v2;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Path;
import android.graphics.RectF;
import android.view.MotionEvent;
import android.view.View;
import java.util.ArrayList;

public class RadialMenuView extends View {
	private ArrayList<RadialMenuItem> mRadialMenuContent = new ArrayList<RadialMenuItem>(0);
	boolean alt;
	float mWidth = -1.0F;

	float mHeight = -1.0F;
	float mThickness;
	float mRadius;
	int selected = -1;

	int lastE = -1;
	float[] endTouch;
	private Paint mBgPaint = new Paint(1);
	private Paint mTextPaint = new Paint(1);
	private Paint mSelectedPaint = new Paint(1);
	private Paint mBorderPaint = new Paint(1);
	private RadialMenuHelperFunctions mHelperFunctions;
	
	private RectF mRectF = new RectF();
	private Path mArc = new Path();
	
	public RadialMenuView(Context context) {
		super(context);
	}

	public RadialMenuView(Context context, RadialMenuRenderer renderer) {
		super(context);
		this.mHelperFunctions = new RadialMenuHelperFunctions();
		this.mRadialMenuContent = renderer.getRadialMenuContent();
		this.alt = renderer.isAlt();
		this.mThickness = renderer.getMenuThickness();
		this.mRadius = renderer.getRadius();
		setVisibility(8);
		initSetPaint(renderer);
	}

	private void initSetPaint(RadialMenuRenderer renderer) {
		this.mBgPaint.setColor(renderer.getMenuBackgroundColor());
		this.mBgPaint.setStrokeWidth(renderer.getMenuThickness());
		this.mBgPaint.setStyle(Paint.Style.STROKE);

		this.mSelectedPaint.setColor(renderer.getMenuSelectedColor());
		this.mSelectedPaint.setStrokeWidth(renderer.getMenuThickness());
		this.mSelectedPaint.setStyle(Paint.Style.STROKE);

		this.mBorderPaint.setColor(renderer.getMenuBorderColor());
		this.mBorderPaint.setStrokeWidth(renderer.getMenuThickness());
		this.mBorderPaint.setStyle(Paint.Style.STROKE);

		this.mTextPaint.setColor(renderer.getMenuTextColor());
		this.mTextPaint.setTextSize(renderer.getMenuThickness() / 2.0F);
	}

	public void setLoc(float x, float y) {
		if (x < this.mRadius + this.mThickness / 2.0F)
			x = this.mRadius + this.mThickness / 2.0F;
		if (y < this.mRadius + this.mThickness / 2.0F) {
			y = this.mRadius + this.mThickness / 2.0F;
		}
		if (y > getHeight() - (this.mRadius + this.mThickness / 2.0F))
			y = getHeight() - (this.mRadius + this.mThickness / 2.0F);
		if (x > getWidth() - (this.mRadius + this.mThickness / 2.0F)) {
			x = getWidth() - (this.mRadius + this.mThickness / 2.0F);
		}
		this.mWidth = x;
		this.mHeight = y;
	}

	public void onDraw(Canvas canvas) {
		setLoc(this.mWidth, this.mHeight);
		mRectF.setEmpty();
		mRectF.set(this.mWidth - this.mRadius, this.mHeight - this.mRadius,
				this.mWidth + this.mRadius, this.mHeight + this.mRadius);
		int tot = this.mRadialMenuContent.size();

		this.mBorderPaint.setStrokeWidth(this.mThickness);

		for (int counter = 0; counter < tot; counter++) {
			if (!((RadialMenuItem) this.mRadialMenuContent.get(counter))
					.equals("HOLLOW")) {
				if (this.alt)
					canvas.drawArc(mRectF, 360 / tot * counter - 90 - 360 / tot
							/ 2, 360 / tot, false,
							this.selected == counter ? this.mSelectedPaint
									: this.mBgPaint);
				else {
					canvas.drawArc(mRectF, 360 / tot * counter - 90, 360 / tot,
							false,
							this.selected == counter ? this.mSelectedPaint
									: this.mBgPaint);
				}
			}
		}
		for (int counter = 0; counter < tot; counter++) {
			if (!((RadialMenuItem) this.mRadialMenuContent.get(counter))
					.equals("HOLLOW")) {
				mArc.reset();
				if (this.alt) {
					mArc.addArc(mRectF, 360 / tot * counter - 90 - 360 / tot / 2
							+ 10.0F, 360 / tot - 10.0F);
					canvas.drawTextOnPath(
							((RadialMenuItem) this.mRadialMenuContent
									.get(counter)).getMenuName(), mArc, 0.0F,
							this.mThickness / 8.0F, this.mTextPaint);
				} else {
					mArc.addArc(mRectF, 360 / tot * counter - 90 + 10.0F,
							360 / tot - 10.0F);
					canvas.drawTextOnPath(
							((RadialMenuItem) this.mRadialMenuContent
									.get(counter)).getMenuName(), mArc, 0.0F,
							-this.mThickness / 8.0F, this.mTextPaint);
				}
			}

		}

		if (tot > 1) {
			for (int counter = 0; counter < tot; counter++) {
				if (!((RadialMenuItem) this.mRadialMenuContent.get(counter))
						.equals("HOLLOW")) {
					if (this.alt) {
						canvas.drawArc(mRectF, 360 / tot * counter - 91 - 360
								/ tot / 2, 2.0F, false, this.mBorderPaint);
						canvas.drawArc(mRectF, 360 / tot * (counter + 1) - 91
								- 360 / tot / 2, 2.0F, false, this.mBorderPaint);
					} else {
						canvas.drawArc(mRectF, 360 / tot * counter - 91, 2.0F,
								false, this.mBorderPaint);
						canvas.drawArc(mRectF, 360 / tot * (counter + 1) - 91,
								2.0F, false, this.mBorderPaint);
					}
				}
			}
		}
		this.mBorderPaint.setStrokeWidth(2.0F);
		mRectF.set(this.mWidth - this.mRadius - this.mThickness / 2.0F,
				this.mHeight - this.mRadius - this.mThickness / 2.0F,
				this.mWidth + this.mRadius + this.mThickness / 2.0F,
				this.mHeight + this.mRadius + this.mThickness / 2.0F);

		for (int counter = 0; counter < tot; counter++) {
			if (!((RadialMenuItem) this.mRadialMenuContent.get(counter))
					.equals("HOLLOW")) {
				if (this.alt)
					canvas.drawArc(mRectF, 360 / tot * counter - 91 - 360 / tot
							/ 2, 360 / tot + 2.0F, false, this.mBorderPaint);
				else {
					canvas.drawArc(mRectF, 360 / tot * counter - 91,
							360 / tot + 2.0F, false, this.mBorderPaint);
				}
			}
		}
		mRectF.set(this.mWidth - this.mRadius + this.mThickness / 2.0F,
				this.mHeight - this.mRadius + this.mThickness / 2.0F,
				this.mWidth + this.mRadius - this.mThickness / 2.0F,
				this.mHeight + this.mRadius - this.mThickness / 2.0F);

		for (int counter = 0; counter < tot; counter++)
			if (!((RadialMenuItem) this.mRadialMenuContent.get(counter))
					.equals("HOLLOW"))
				if (this.alt)
					canvas.drawArc(mRectF, 360 / tot * counter - 91 - 360 / tot
							/ 2, 360 / tot + 1.0F, false, this.mBorderPaint);
				else
					canvas.drawArc(mRectF, 360 / tot * counter - 91,
							360 / tot + 1.0F, false, this.mBorderPaint);
	}

	private boolean handleEvent(int e) {
		if (e == this.mRadialMenuContent.size()) {
			e = 0;
		} else if (e == -1) {
			this.selected = -1;
			return false;
		}
		if (((RadialMenuItem) this.mRadialMenuContent.get(e)).getMenuName()
				.equals("HOLLOW")) {
			this.selected = -1;
			invalidate();
			return false;
		}
		((RadialMenuItem) this.mRadialMenuContent.get(e))
				.getOnRadailMenuClick().onRadialMenuClick(
						((RadialMenuItem) this.mRadialMenuContent.get(e))
								.getMenuID());
		this.selected = -1;
		invalidate();
		return true;
	}

	private void preEvent(int e) {
		if (e == this.mRadialMenuContent.size())
			e = 0;
		else if (this.lastE == e)
			return;
		this.lastE = e;
		if (e == -1) {
			this.selected = -1;
			invalidate();
			return;
		}
		if (((RadialMenuItem) this.mRadialMenuContent.get(e)).getMenuName()
				.equals("HOLLOW")) {
			this.selected = -1;
			invalidate();
			return;
		}
		this.selected = e;
		invalidate();
	}

	public boolean gestureHandler(MotionEvent event, boolean eat) {
		if (event.getAction() == 1) {
			this.endTouch = new float[] { event.getX(), event.getY() };
			if (this.mHelperFunctions.distance(this.mWidth, this.mHeight,
					this.endTouch[0], this.endTouch[1]) > this.mRadius
					- this.mThickness / 2.0F) {
				setVisibility(8);
				return handleEvent((int) this.mHelperFunctions.angle(
						this.mWidth, this.mHeight, this.endTouch[0],
						this.endTouch[1], this.alt,
						this.mRadialMenuContent.size()));
			}
			setVisibility(8);
			return handleEvent(-1);
		}
		if (event.getAction() == 0) {
			this.mWidth = event.getX();
			this.mHeight = event.getY();
			setVisibility(0);
			invalidate();
		} else if (event.getAction() == 2) {
			this.endTouch = new float[] { event.getX(), event.getY() };
			if (this.mHelperFunctions.distance(this.mWidth, this.mHeight,
					this.endTouch[0], this.endTouch[1]) > this.mRadius
					- this.mThickness / 2.0F)
				preEvent((int) this.mHelperFunctions.angle(this.mWidth,
						this.mHeight, this.endTouch[0], this.endTouch[1],
						this.alt, this.mRadialMenuContent.size()));
			else {
				preEvent(-1);
			}
		}

		return eat;
	}
}