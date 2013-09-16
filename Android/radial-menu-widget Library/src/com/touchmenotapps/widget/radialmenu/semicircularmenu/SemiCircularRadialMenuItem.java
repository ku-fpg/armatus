package com.touchmenotapps.widget.radialmenu.semicircularmenu;

import android.graphics.Path;
import android.graphics.Point;
import android.graphics.RectF;
import android.graphics.drawable.Drawable;

public class SemiCircularRadialMenuItem {
	private String mMenuID;
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

	public SemiCircularRadialMenuItem(String id, Drawable mIcon, String mText) {
		this.mMenuID = id;
		this.mIcon = mIcon;
		this.mText = mText;
		this.mMenuNormalColor = -1;
		this.mMenuSelectedColor = -3355444;
		this.mBackgroundColor = this.mMenuNormalColor;
		this.mTextColor = -16777216;
		this.mIconDimen = 64;
		this.mPath = new Path();
		this.mBounds = new RectF();
	}

	public int getTextColor() {
		return this.mTextColor;
	}

	public void setTextColor(int mTextColor) {
		this.mTextColor = mTextColor;
	}

	public String getMenuID() {
		return this.mMenuID;
	}

	public int getIconDimen() {
		return this.mIconDimen;
	}

	public void setIconDimen(int mIconDimen) {
		this.mIconDimen = mIconDimen;
	}

	public Path getMenuPath() {
		return this.mPath;
	}

	public RectF getBounds() {
		return this.mBounds;
	}

	public void setMenuPath(RectF menuButtonRect, RectF menuRect,
			float StartArc, float ArcWidth, float radius, Point anchorPoint) {
		int left = 0;

		this.mPath.arcTo(menuRect, StartArc, ArcWidth);
		this.mPath.arcTo(menuButtonRect, StartArc + ArcWidth, -ArcWidth);
		this.mPath.close();
		this.mPath.computeBounds(this.mBounds, true);

		Point drawableCenter = pointOnCircle(radius - radius / 5.0F, StartArc
				+ ArcWidth / 2.0F, anchorPoint);
		left = drawableCenter.x - this.mIconDimen / 2;
		int top = drawableCenter.y - this.mIconDimen / 2;
		int right = left + this.mIconDimen;
		int bottom = top + this.mIconDimen;
		this.mIcon.setBounds(left, top, right, bottom);
	}

	private Point pointOnCircle(float radius, float angleInDegrees, Point origin) {
		int x = (int) (radius * Math
				.cos(angleInDegrees * 3.141592653589793D / 180.0D)) + origin.x;
		int y = (int) (radius * Math
				.sin(angleInDegrees * 3.141592653589793D / 180.0D)) + origin.y;
		return new Point(x, y);
	}

	public Drawable getIcon() {
		return this.mIcon;
	}

	public String getText() {
		return this.mText;
	}

	public void setOnSemiCircularRadialMenuPressed(
			OnSemiCircularRadialMenuPressed mCallback) {
		this.mCallback = mCallback;
	}

	public OnSemiCircularRadialMenuPressed getCallback() {
		return this.mCallback;
	}

	public int getBackgroundColor() {
		return this.mBackgroundColor;
	}

	public void setBackgroundColor(int color) {
		this.mBackgroundColor = color;
	}

	public int getMenuNormalColor() {
		return this.mMenuNormalColor;
	}

	public void setMenuNormalColor(int mMenuNormalColor) {
		this.mMenuNormalColor = mMenuNormalColor;
	}

	public int getMenuSelectedColor() {
		return this.mMenuSelectedColor;
	}

	public void setMenuSelectedColor(int mMenuSelectedColor) {
		this.mMenuSelectedColor = mMenuSelectedColor;
	}

	public static abstract interface OnSemiCircularRadialMenuPressed {
		public abstract void onMenuItemPressed();
	}
}