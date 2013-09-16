package com.touchmenotapps.widget.radialmenu.semicircularmenu;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.graphics.Rect;
import android.graphics.RectF;
import android.util.AttributeSet;
import android.util.Log;
import android.view.MotionEvent;
import android.view.View;
import java.util.HashMap;

public class SemiCircularRadialMenu extends View {
	public static final int VERTICAL_RIGHT = 0;
	public static final int VERTICAL_LEFT = 1;
	public static final int HORIZONTAL_TOP = 2;
	public static final int HORIZONTAL_BOTTOM = 3;
	private boolean isMenuVisible = false;
	private boolean isMenuTogglePressed = false;
	private boolean isMenuItemPressed = false;
	private String mPressedMenuItemID = null;
	private int mDiameter = 0;
	private float mRadius = 0.0F;
	private int mStartAngle = 0;
	private RectF mMenuRect;
	private RectF mMenuCenterButtonRect;
	private Paint mRadialMenuPaint = new Paint(1);
	private Point mViewAnchorPoints;
	private HashMap<String, SemiCircularRadialMenuItem> mMenuItems = new HashMap<String, SemiCircularRadialMenuItem>();

	private float mShadowRadius = 5.0F * getResources().getDisplayMetrics().density;
	private boolean isShowMenuText = false;
	private int mOrientation = 3;
	private int centerRadialColor = -1;
	private int mShadowColor = -7829368;
	private String openMenuText = "Open";
	private String closeMenuText = "Close";
	private String centerMenuText = this.openMenuText;
	private int mToggleMenuTextColor = -12303292;
	private float textSize = 12.0F * getResources().getDisplayMetrics().density;
	private int mOpenButtonScaleFactor = 3;

	public SemiCircularRadialMenu(Context context) {
		super(context);
		init();
	}

	public SemiCircularRadialMenu(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public SemiCircularRadialMenu(Context context, AttributeSet attrs,
			int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	private void init() {
		this.mRadialMenuPaint.setTextSize(this.textSize);
		this.mRadialMenuPaint.setColor(-1);
	}

	protected void onDraw(Canvas canvas) {
		super.onDraw(canvas);
		this.mRadialMenuPaint.setShadowLayer(this.mShadowRadius, 0.0F, 0.0F,
				this.mShadowColor);

		if (this.isMenuVisible) {
			canvas.drawArc(this.mMenuRect, this.mStartAngle, 180.0F, true,
					this.mRadialMenuPaint);

			if (this.mMenuItems.size() > 0) {
				float mStart = this.mStartAngle;

				float mSweep = 180 / this.mMenuItems.size();
				for (SemiCircularRadialMenuItem item : this.mMenuItems.values()) {
					this.mRadialMenuPaint.setColor(item.getBackgroundColor());
					item.setMenuPath(this.mMenuCenterButtonRect,
							this.mMenuRect, mStart, mSweep, this.mRadius,
							this.mViewAnchorPoints);
					canvas.drawPath(item.getMenuPath(), this.mRadialMenuPaint);
					if (this.isShowMenuText) {
						this.mRadialMenuPaint.setShadowLayer(
								this.mShadowRadius, 0.0F, 0.0F, 0);
						this.mRadialMenuPaint.setColor(item.getTextColor());
						canvas.drawTextOnPath(item.getText(),
								item.getMenuPath(), 5.0F, this.textSize,
								this.mRadialMenuPaint);
						this.mRadialMenuPaint.setShadowLayer(
								this.mShadowRadius, 0.0F, 0.0F,
								this.mShadowColor);
					}
					item.getIcon().draw(canvas);
					mStart += mSweep;
				}
				this.mRadialMenuPaint.setStyle(Paint.Style.FILL);
			}
		}

		this.mRadialMenuPaint.setColor(this.centerRadialColor);
		canvas.drawArc(this.mMenuCenterButtonRect, this.mStartAngle, 180.0F,
				true, this.mRadialMenuPaint);
		this.mRadialMenuPaint.setShadowLayer(this.mShadowRadius, 0.0F, 0.0F, 0);

		drawCenterText(canvas, this.mRadialMenuPaint);
	}

	public boolean onTouchEvent(MotionEvent event) {
		int x = (int) event.getX();
		int y = (int) event.getY();

		switch (event.getAction()) {
		case 0:
			if (this.mMenuCenterButtonRect.contains(x, y)) {
				this.centerRadialColor = -13388315;
				this.isMenuTogglePressed = true;
				invalidate();
			} else if ((this.isMenuVisible) && (this.mMenuItems.size() > 0)) {
				for (SemiCircularRadialMenuItem item : this.mMenuItems.values()) {
					if ((this.mMenuRect.contains(x, y))
							&& (item.getBounds().contains(x, y))) {
						this.isMenuItemPressed = true;
						this.mPressedMenuItemID = item.getMenuID();
						break;
					}
				}
				((SemiCircularRadialMenuItem) this.mMenuItems
						.get(this.mPressedMenuItemID))
						.setBackgroundColor(((SemiCircularRadialMenuItem) this.mMenuItems
								.get(this.mPressedMenuItemID))
								.getMenuSelectedColor());
				invalidate();
			}

			break;
		case 1:
			if (this.isMenuTogglePressed) {
				this.centerRadialColor = -1;
				if (this.isMenuVisible) {
					this.isMenuVisible = false;
					this.centerMenuText = this.openMenuText;
				} else {
					this.isMenuVisible = true;
					this.centerMenuText = this.closeMenuText;
				}
				this.isMenuTogglePressed = false;
				invalidate();
			}

			if (this.isMenuItemPressed) {
				if (((SemiCircularRadialMenuItem) this.mMenuItems
						.get(this.mPressedMenuItemID)).getCallback() != null) {
					((SemiCircularRadialMenuItem) this.mMenuItems
							.get(this.mPressedMenuItemID)).getCallback()
							.onMenuItemPressed();
				}
				((SemiCircularRadialMenuItem) this.mMenuItems
						.get(this.mPressedMenuItemID))
						.setBackgroundColor(((SemiCircularRadialMenuItem) this.mMenuItems
								.get(this.mPressedMenuItemID))
								.getMenuNormalColor());
				this.isMenuItemPressed = false;
				invalidate();
			}
			break;
		}

		return true;
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

		this.mMenuRect = getRadialMenuRect(false);
		this.mMenuCenterButtonRect = getRadialMenuRect(true);
	}

	private void drawCenterText(Canvas canvas, Paint paint) {
		paint.setColor(this.mToggleMenuTextColor);
		switch (this.mOrientation) {
		case 0:
			canvas.drawText(this.centerMenuText,
					getWidth() - paint.measureText(this.centerMenuText),
					getHeight() / 2, paint);
			break;
		case 1:
			canvas.drawText(this.centerMenuText, 2.0F, getHeight() / 2, paint);
			break;
		case 2:
			canvas.drawText(this.centerMenuText,
					getWidth() / 2 - paint.measureText(this.centerMenuText)
							/ 2.0F, this.textSize, paint);
			break;
		case 3:
			canvas.drawText(this.centerMenuText,
					getWidth() / 2 - paint.measureText(this.centerMenuText)
							/ 2.0F, getHeight() - this.textSize, paint);
		}
	}

	private RectF getRadialMenuRect(boolean isCenterButton) {
		int bottom;
		int top;
		int right;
		int left = right = top = bottom = 0;
		switch (this.mOrientation) {
		case 0:
			if (isCenterButton) {
				left = getWidth()
						- (int) (this.mRadius / this.mOpenButtonScaleFactor);
				right = getWidth()
						+ (int) (this.mRadius / this.mOpenButtonScaleFactor);
				top = getHeight() / 2
						- (int) (this.mRadius / this.mOpenButtonScaleFactor);
				bottom = getHeight() / 2
						+ (int) (this.mRadius / this.mOpenButtonScaleFactor);
			} else {
				left = getWidth() - (int) this.mRadius;
				right = getWidth() + (int) this.mRadius;
				top = getHeight() / 2 - (int) this.mRadius;
				bottom = getHeight() / 2 + (int) this.mRadius;
			}
			this.mStartAngle = 90;
			this.mViewAnchorPoints = new Point(getWidth(), getHeight() / 2);
			break;
		case 1:
			if (isCenterButton) {
				left = -(int) (this.mRadius / this.mOpenButtonScaleFactor);
				right = (int) (this.mRadius / this.mOpenButtonScaleFactor);
				top = getHeight() / 2
						- (int) (this.mRadius / this.mOpenButtonScaleFactor);
				bottom = getHeight() / 2
						+ (int) (this.mRadius / this.mOpenButtonScaleFactor);
			} else {
				left = -(int) this.mRadius;
				right = (int) this.mRadius;
				top = getHeight() / 2 - (int) this.mRadius;
				bottom = getHeight() / 2 + (int) this.mRadius;
			}
			this.mStartAngle = 270;
			this.mViewAnchorPoints = new Point(0, getHeight() / 2);
			break;
		case 2:
			if (isCenterButton) {
				left = getWidth() / 2
						- (int) (this.mRadius / this.mOpenButtonScaleFactor);
				right = getWidth() / 2
						+ (int) (this.mRadius / this.mOpenButtonScaleFactor);
				top = -(int) (this.mRadius / this.mOpenButtonScaleFactor);
				bottom = (int) (this.mRadius / this.mOpenButtonScaleFactor);
			} else {
				left = getWidth() / 2 - (int) this.mRadius;
				right = getWidth() / 2 + (int) this.mRadius;
				top = -(int) this.mRadius;
				bottom = (int) this.mRadius;
			}
			this.mStartAngle = 0;
			this.mViewAnchorPoints = new Point(getWidth() / 2, 0);
			break;
		case 3:
			if (isCenterButton) {
				left = getWidth() / 2
						- (int) (this.mRadius / this.mOpenButtonScaleFactor);
				right = getWidth() / 2
						+ (int) (this.mRadius / this.mOpenButtonScaleFactor);
				top = getHeight()
						- (int) (this.mRadius / this.mOpenButtonScaleFactor);
				bottom = getHeight()
						+ (int) (this.mRadius / this.mOpenButtonScaleFactor);
			} else {
				left = getWidth() / 2 - (int) this.mRadius;
				right = getWidth() / 2 + (int) this.mRadius;
				top = getHeight() - (int) this.mRadius;
				bottom = getHeight() + (int) this.mRadius;
			}
			this.mStartAngle = 180;
			this.mViewAnchorPoints = new Point(getWidth() / 2, getHeight());
		}

		Rect rect = new Rect(left, top, right, bottom);
		Log.i("View", " Top " + top + " Bottom " + bottom + " Left " + left
				+ "  Right " + right);
		return new RectF(rect);
	}

	public void setOrientation(int orientation) {
		this.mOrientation = orientation;
		this.mMenuRect = getRadialMenuRect(false);
		this.mMenuCenterButtonRect = getRadialMenuRect(true);
		invalidate();
	}

	public void addMenuItem(String idTag, SemiCircularRadialMenuItem mMenuItem) {
		this.mMenuItems.put(idTag, mMenuItem);
		invalidate();
	}

	public void removeMenuItemById(String idTag) {
		this.mMenuItems.remove(idTag);
		invalidate();
	}

	public void removeAllMenuItems() {
		this.mMenuItems.clear();
		invalidate();
	}

	public void dismissMenu() {
		this.isMenuVisible = false;
		this.centerMenuText = this.openMenuText;
		invalidate();
	}

	public float getShadowRadius() {
		return this.mShadowRadius;
	}

	public void setShadowRadius(int mShadowRadius) {
		this.mShadowRadius = (mShadowRadius * getResources()
				.getDisplayMetrics().density);
		invalidate();
	}

	public boolean isShowMenuText() {
		return this.isShowMenuText;
	}

	public void setShowMenuText(boolean isShowMenuText) {
		this.isShowMenuText = isShowMenuText;
		invalidate();
	}

	public int getOrientation() {
		return this.mOrientation;
	}

	public int getCenterRadialColor() {
		return this.centerRadialColor;
	}

	public void setCenterRadialColor(int centerRadialColor) {
		this.centerRadialColor = centerRadialColor;
		invalidate();
	}

	public int getShadowColor() {
		return this.mShadowColor;
	}

	public void setShadowColor(int mShadowColor) {
		this.mShadowColor = mShadowColor;
		invalidate();
	}

	public String getOpenMenuText() {
		return this.openMenuText;
	}

	public void setOpenMenuText(String openMenuText) {
		this.openMenuText = openMenuText;
		if (!this.isMenuTogglePressed)
			this.centerMenuText = openMenuText;
		invalidate();
	}

	public String getCloseMenuText() {
		return this.closeMenuText;
	}

	public void setCloseMenuText(String closeMenuText) {
		this.closeMenuText = closeMenuText;
		if (this.isMenuTogglePressed)
			this.centerMenuText = closeMenuText;
		invalidate();
	}

	public int getToggleMenuTextColor() {
		return this.mToggleMenuTextColor;
	}

	public void setToggleMenuTextColor(int mToggleMenuTextColor) {
		this.mToggleMenuTextColor = mToggleMenuTextColor;
		invalidate();
	}

	public float getTextSize() {
		return this.textSize;
	}

	public void setTextSize(int textSize) {
		this.textSize = (textSize * getResources().getDisplayMetrics().density);
		this.mRadialMenuPaint.setTextSize(this.textSize);
		invalidate();
	}

	public int getOpenButtonScaleFactor() {
		return this.mOpenButtonScaleFactor;
	}

	public void setOpenButtonScaleFactor(int mOpenButtonScaleFactor) {
		this.mOpenButtonScaleFactor = mOpenButtonScaleFactor;
		invalidate();
	}
}