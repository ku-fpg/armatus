package com.touchmenotapps.widget.radialmenu.semicircularmenu;

import android.graphics.Path;
import android.graphics.Point;
import android.graphics.RectF;
import android.graphics.drawable.Drawable;

public class SemiCircularRadialMenuItem {
	private String mMenuId;
	private Drawable mIcon;
	private String mText;
	private int mBackgroundColor;
	private int mMenuNormalColor;
	private int mMenuSelectedColor;
	private int mTextColor;
	private Path mPath;
	private RectF mBounds;
	private OnSemiCircularRadialMenuPressed mCallback;
	private int mIconDimen;

	public SemiCircularRadialMenuItem(String id, Drawable icon, String text) {
		mMenuId = id;
		mIcon = icon;
		mText = text;
		mMenuNormalColor = -1;
		mMenuSelectedColor = -3355444;
		mBackgroundColor = mMenuNormalColor;
		mTextColor = -16777216;
		mIconDimen = 64;
		mPath = new Path();
		mBounds = new RectF();
	}

	public int getTextColor() {
		return mTextColor;
	}

	public void setTextColor(int textColor) {
		mTextColor = textColor;
	}

	public String getMenuID() {
		return mMenuId;
	}

	public int getIconDimen() {
		return mIconDimen;
	}

	public void setIconDimen(int iconDimen) {
		mIconDimen = iconDimen;
	}

	public Path getMenuPath() {
		return mPath;
	}

	public RectF getBounds() {
		return mBounds;
	}

	public void setMenuPath(RectF menuButtonRect, RectF menuRect,
			float startArc, float arcWidth, float radius, Point anchorPoint) {
		int left = 0;

		mPath.arcTo(menuRect, startArc, arcWidth);
		mPath.arcTo(menuButtonRect, startArc + arcWidth, -arcWidth);
		mPath.close();
		mPath.computeBounds(mBounds, true);

		Point drawableCenter = pointOnCircle(radius - radius / 5.0F, startArc
				+ arcWidth / 2.0F, anchorPoint);
		left = drawableCenter.x - mIconDimen / 2;
		int top = drawableCenter.y - mIconDimen / 2;
		int right = left + mIconDimen;
		int bottom = top + mIconDimen;
		mIcon.setBounds(left, top, right, bottom);
	}

	private Point pointOnCircle(float radius, float angleInDegrees, Point origin) {
		int x = (int) (radius * Math
				.cos(angleInDegrees * 3.141592653589793D / 180.0D)) + origin.x;
		int y = (int) (radius * Math
				.sin(angleInDegrees * 3.141592653589793D / 180.0D)) + origin.y;
		return new Point(x, y);
	}

	public Drawable getIcon() {
		return mIcon;
	}

	public String getText() {
		return mText;
	}

	public void setOnSemiCircularRadialMenuPressed(OnSemiCircularRadialMenuPressed callback) {
		mCallback = callback;
	}

	public OnSemiCircularRadialMenuPressed getCallback() {
		return mCallback;
	}

	public int getBackgroundColor() {
		return mBackgroundColor;
	}

	public void setBackgroundColor(int color) {
		mBackgroundColor = color;
	}

	public int getMenuNormalColor() {
		return mMenuNormalColor;
	}

	public void setMenuNormalColor(int menuNormalColor) {
		mMenuNormalColor = menuNormalColor;
	}

	public int getMenuSelectedColor() {
		return mMenuSelectedColor;
	}

	public void setMenuSelectedColor(int menuSelectedColor) {
		mMenuSelectedColor = menuSelectedColor;
	}

	public static abstract interface OnSemiCircularRadialMenuPressed {
		public abstract void onMenuItemPressed();
	}
}