package com.touchmenotapps.widget.radialmenu.menu.v2;

import android.view.MotionEvent;
import android.view.View;
import java.util.ArrayList;

public class RadialMenuRenderer {
	public static final String RADIAL_NO_TEXT = "HOLLOW";
	private ArrayList<RadialMenuItem> mRadialMenuContent = new ArrayList<RadialMenuItem>(0);

	private boolean alt = false;

	private float mThickness = 30.0F;

	private float mRadius = 60.0F;

	private int mMenuBackgroundColor = -2143009724;

	private int mMenuSelectedColor = -2144094747;

	private int mMenuTextColor = -1;

	private int mMenuBorderColor = -8947849;
	private View mParentView;

	public RadialMenuRenderer(View parentView, boolean alt, float mThick,
			float mRadius) {
		this.mParentView = parentView;
		this.alt = alt;
		this.mThickness = mThick;
		this.mRadius = mRadius;
	}

	public void setRadialMenuContent(
			ArrayList<RadialMenuItem> mRadialMenuContent) {
		this.mRadialMenuContent = mRadialMenuContent;
	}

	public View renderView() {
		final RadialMenuView menu = new RadialMenuView(
				this.mParentView.getContext(), this);
		this.mParentView.setOnTouchListener(new View.OnTouchListener() {
			public boolean onTouch(View v, MotionEvent event) {
				return menu.gestureHandler(event, true);
			}
		});
		return menu;
	}

	public ArrayList<RadialMenuItem> getRadialMenuContent() {
		return this.mRadialMenuContent;
	}

	public boolean isAlt() {
		return this.alt;
	}

	public float getMenuThickness() {
		return this.mThickness;
	}

	public float getRadius() {
		return this.mRadius;
	}

	public int getMenuBackgroundColor() {
		return this.mMenuBackgroundColor;
	}

	public void setMenuBackgroundColor(int mMenuBackgroundColor) {
		this.mMenuBackgroundColor = mMenuBackgroundColor;
	}

	public int getMenuSelectedColor() {
		return this.mMenuSelectedColor;
	}

	public void setMenuSelectedColor(int mMenuSelectedColor) {
		this.mMenuSelectedColor = mMenuSelectedColor;
	}

	public int getMenuTextColor() {
		return this.mMenuTextColor;
	}

	public void setMenuTextColor(int mMenuTextColor) {
		this.mMenuTextColor = mMenuTextColor;
	}

	public int getMenuBorderColor() {
		return this.mMenuBorderColor;
	}

	public void setMenuBorderColor(int mMenuBorderColor) {
		this.mMenuBorderColor = mMenuBorderColor;
	}

	public static abstract interface OnRadialMenuClickListener {
		public abstract void onRadialMenuClick(String paramString);
	}
}