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
	
	private boolean mIsMenuVisible = false;
	private boolean mIsMenuTogglePressed = false;
	private boolean mIsMenuItemPressed = false;
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
	private boolean mIsShowMenuText = false;
	private int mOrientation = 3;
	private int mCenterRadialColor = -1;
	private int mShadowColor = -7829368;
	private String mOpenMenuText = "Open";
	private String mCloseMenuText = "Close";
	private String mCenterMenuText = mOpenMenuText;
	private int mToggleMenuTextColor = -12303292;
	private float mTextSize = 12.0F * getResources().getDisplayMetrics().density;
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
		mRadialMenuPaint.setTextSize(mTextSize);
		mRadialMenuPaint.setColor(-1);
	}

	protected void onDraw(Canvas canvas) {
		super.onDraw(canvas);
		mRadialMenuPaint.setShadowLayer(mShadowRadius, 0.0F, 0.0F,
				mShadowColor);

		if (mIsMenuVisible) {
			canvas.drawArc(mMenuRect, mStartAngle, 180.0F, true,
					mRadialMenuPaint);

			if (mMenuItems.size() > 0) {
				float mStart = mStartAngle;

				float mSweep = 180 / mMenuItems.size();
				for (SemiCircularRadialMenuItem item : mMenuItems.values()) {
					mRadialMenuPaint.setColor(item.getBackgroundColor());
					item.setMenuPath(mMenuCenterButtonRect,
							mMenuRect, mStart, mSweep, mRadius,
							mViewAnchorPoints);
					canvas.drawPath(item.getMenuPath(), mRadialMenuPaint);
					if (mIsShowMenuText) {
						mRadialMenuPaint.setShadowLayer(
								mShadowRadius, 0.0F, 0.0F, 0);
						mRadialMenuPaint.setColor(item.getTextColor());
						canvas.drawTextOnPath(item.getText(),
								item.getMenuPath(), 5.0F, mTextSize,
								mRadialMenuPaint);
						mRadialMenuPaint.setShadowLayer(
								mShadowRadius, 0.0F, 0.0F,
								mShadowColor);
					}
					item.getIcon().draw(canvas);
					mStart += mSweep;
				}
				mRadialMenuPaint.setStyle(Paint.Style.FILL);
			}
		}

		mRadialMenuPaint.setColor(mCenterRadialColor);
		canvas.drawArc(mMenuCenterButtonRect, mStartAngle, 180.0F,
				true, mRadialMenuPaint);
		mRadialMenuPaint.setShadowLayer(mShadowRadius, 0.0F, 0.0F, 0);

		drawCenterText(canvas, mRadialMenuPaint);
	}

	public boolean onTouchEvent(MotionEvent event) {
		int x = (int) event.getX();
		int y = (int) event.getY();

		switch (event.getAction()) {
		case MotionEvent.ACTION_DOWN:
			if (mMenuCenterButtonRect.contains(x, y)) {
				mCenterRadialColor = -13388315;
				mIsMenuTogglePressed = true;
				invalidate();
			} else if ((mIsMenuVisible) && (mMenuItems.size() > 0)) {
				for (SemiCircularRadialMenuItem item : mMenuItems.values()) {
					if ((mMenuRect.contains(x, y))
							&& (item.getBounds().contains(x, y))) {
						mIsMenuItemPressed = true;
						mPressedMenuItemID = item.getMenuID();
						break;
					}
				}
				((SemiCircularRadialMenuItem) mMenuItems
						.get(mPressedMenuItemID))
						.setBackgroundColor(((SemiCircularRadialMenuItem) mMenuItems
								.get(mPressedMenuItemID))
								.getMenuSelectedColor());
				invalidate();
			}

			break;
		case MotionEvent.ACTION_UP:
			if (mIsMenuTogglePressed) {
				mCenterRadialColor = -1;
				if (mIsMenuVisible) {
					mIsMenuVisible = false;
					mCenterMenuText = mOpenMenuText;
				} else {
					mIsMenuVisible = true;
					mCenterMenuText = mCloseMenuText;
				}
				mIsMenuTogglePressed = false;
				invalidate();
			}

			if (mIsMenuItemPressed) {
				if (((SemiCircularRadialMenuItem) mMenuItems
						.get(mPressedMenuItemID)).getCallback() != null) {
					((SemiCircularRadialMenuItem) mMenuItems
							.get(mPressedMenuItemID)).getCallback()
							.onMenuItemPressed();
				}
				((SemiCircularRadialMenuItem) mMenuItems
						.get(mPressedMenuItemID))
						.setBackgroundColor(((SemiCircularRadialMenuItem) mMenuItems
								.get(mPressedMenuItemID))
								.getMenuNormalColor());
				mIsMenuItemPressed = false;
				invalidate();
			}
			break;
		}

		return true;
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

		mMenuRect = getRadialMenuRect(false);
		mMenuCenterButtonRect = getRadialMenuRect(true);
	}

	private void drawCenterText(Canvas canvas, Paint paint) {
		paint.setColor(mToggleMenuTextColor);
		switch (mOrientation) {
		case 0:
			canvas.drawText(mCenterMenuText,
					getWidth() - paint.measureText(mCenterMenuText),
					getHeight() / 2, paint);
			break;
		case 1:
			canvas.drawText(mCenterMenuText, 2.0F, getHeight() / 2, paint);
			break;
		case 2:
			canvas.drawText(mCenterMenuText,
					getWidth() / 2 - paint.measureText(mCenterMenuText)
							/ 2.0F, mTextSize, paint);
			break;
		case 3:
			canvas.drawText(mCenterMenuText,
					getWidth() / 2 - paint.measureText(mCenterMenuText)
							/ 2.0F, getHeight() - mTextSize, paint);
		}
	}

	private RectF getRadialMenuRect(boolean isCenterButton) {
		int bottom;
		int top;
		int right;
		int left = right = top = bottom = 0;
		switch (mOrientation) {
		case 0:
			if (isCenterButton) {
				left = getWidth()
						- (int) (mRadius / mOpenButtonScaleFactor);
				right = getWidth()
						+ (int) (mRadius / mOpenButtonScaleFactor);
				top = getHeight() / 2
						- (int) (mRadius / mOpenButtonScaleFactor);
				bottom = getHeight() / 2
						+ (int) (mRadius / mOpenButtonScaleFactor);
			} else {
				left = getWidth() - (int) mRadius;
				right = getWidth() + (int) mRadius;
				top = getHeight() / 2 - (int) mRadius;
				bottom = getHeight() / 2 + (int) mRadius;
			}
			mStartAngle = 90;
			mViewAnchorPoints = new Point(getWidth(), getHeight() / 2);
			break;
		case 1:
			if (isCenterButton) {
				left = -(int) (mRadius / mOpenButtonScaleFactor);
				right = (int) (mRadius / mOpenButtonScaleFactor);
				top = getHeight() / 2
						- (int) (mRadius / mOpenButtonScaleFactor);
				bottom = getHeight() / 2
						+ (int) (mRadius / mOpenButtonScaleFactor);
			} else {
				left = -(int) mRadius;
				right = (int) mRadius;
				top = getHeight() / 2 - (int) mRadius;
				bottom = getHeight() / 2 + (int) mRadius;
			}
			mStartAngle = 270;
			mViewAnchorPoints = new Point(0, getHeight() / 2);
			break;
		case 2:
			if (isCenterButton) {
				left = getWidth() / 2
						- (int) (mRadius / mOpenButtonScaleFactor);
				right = getWidth() / 2
						+ (int) (mRadius / mOpenButtonScaleFactor);
				top = -(int) (mRadius / mOpenButtonScaleFactor);
				bottom = (int) (mRadius / mOpenButtonScaleFactor);
			} else {
				left = getWidth() / 2 - (int) mRadius;
				right = getWidth() / 2 + (int) mRadius;
				top = -(int) mRadius;
				bottom = (int) mRadius;
			}
			mStartAngle = 0;
			mViewAnchorPoints = new Point(getWidth() / 2, 0);
			break;
		case 3:
			if (isCenterButton) {
				left = getWidth() / 2
						- (int) (mRadius / mOpenButtonScaleFactor);
				right = getWidth() / 2
						+ (int) (mRadius / mOpenButtonScaleFactor);
				top = getHeight()
						- (int) (mRadius / mOpenButtonScaleFactor);
				bottom = getHeight()
						+ (int) (mRadius / mOpenButtonScaleFactor);
			} else {
				left = getWidth() / 2 - (int) mRadius;
				right = getWidth() / 2 + (int) mRadius;
				top = getHeight() - (int) mRadius;
				bottom = getHeight() + (int) mRadius;
			}
			mStartAngle = 180;
			mViewAnchorPoints = new Point(getWidth() / 2, getHeight());
		}

		Rect rect = new Rect(left, top, right, bottom);
		Log.i("View", " Top " + top + " Bottom " + bottom + " Left " + left
				+ "  Right " + right);
		return new RectF(rect);
	}

	public void setOrientation(int orientation) {
		mOrientation = orientation;
		mMenuRect = getRadialMenuRect(false);
		mMenuCenterButtonRect = getRadialMenuRect(true);
		invalidate();
	}

	public void addMenuItem(String idTag, SemiCircularRadialMenuItem menuItem) {
		mMenuItems.put(idTag, menuItem);
		invalidate();
	}

	public void removeMenuItemById(String idTag) {
		mMenuItems.remove(idTag);
		invalidate();
	}

	public void removeAllMenuItems() {
		mMenuItems.clear();
		invalidate();
	}

	public void dismissMenu() {
		mIsMenuVisible = false;
		mCenterMenuText = mOpenMenuText;
		invalidate();
	}

	public float getShadowRadius() {
		return mShadowRadius;
	}

	public void setShadowRadius(int shadowRadius) {
		mShadowRadius = (shadowRadius * getResources()
				.getDisplayMetrics().density);
		invalidate();
	}

	public boolean isShowMenuText() {
		return mIsShowMenuText;
	}

	public void setShowMenuText(boolean isShowMenuText) {
		mIsShowMenuText = isShowMenuText;
		invalidate();
	}

	public int getOrientation() {
		return mOrientation;
	}

	public int getCenterRadialColor() {
		return mCenterRadialColor;
	}

	public void setCenterRadialColor(int centerRadialColor) {
		mCenterRadialColor = centerRadialColor;
		invalidate();
	}

	public int getShadowColor() {
		return mShadowColor;
	}

	public void setShadowColor(int shadowColor) {
		mShadowColor = shadowColor;
		invalidate();
	}

	public String getOpenMenuText() {
		return mOpenMenuText;
	}

	public void setOpenMenuText(String openMenuText) {
		mOpenMenuText = openMenuText;
		if (!mIsMenuTogglePressed)
			mCenterMenuText = openMenuText;
		invalidate();
	}

	public String getCloseMenuText() {
		return mCloseMenuText;
	}

	public void setCloseMenuText(String closeMenuText) {
		mCloseMenuText = closeMenuText;
		if (mIsMenuTogglePressed)
			mCenterMenuText = closeMenuText;
		invalidate();
	}

	public int getToggleMenuTextColor() {
		return mToggleMenuTextColor;
	}

	public void setToggleMenuTextColor(int toggleMenuTextColor) {
		mToggleMenuTextColor = toggleMenuTextColor;
		invalidate();
	}

	public float getTextSize() {
		return mTextSize;
	}

	public void setTextSize(int textSize) {
		mTextSize = (textSize * getResources().getDisplayMetrics().density);
		mRadialMenuPaint.setTextSize(mTextSize);
		invalidate();
	}

	public int getOpenButtonScaleFactor() {
		return mOpenButtonScaleFactor;
	}

	public void setOpenButtonScaleFactor(int openButtonScaleFactor) {
		mOpenButtonScaleFactor = openButtonScaleFactor;
		invalidate();
	}
}