package com.touchmenotapps.widget.radialmenu.menu.v2;

import android.view.MotionEvent;
import android.view.View;
import java.util.ArrayList;
import java.util.List;

public class RadialMenuRenderer {
	public static final String RADIAL_NO_TEXT = "HOLLOW";
	private List<RadialMenuItem> mRadialMenuContent = new ArrayList<RadialMenuItem>(0);
	private boolean mAlt = false;
	private float mThickness = 30.0F;
	private float mRadius = 60.0F;
	private int mMenuBackgroundColor = -2143009724;
	private int mMenuSelectedColor = -2144094747;
	private int mMenuTextColor = -1;
	private int mMenuBorderColor = -8947849;
	private View mParentView;

	public RadialMenuRenderer(View parentView, boolean alt, float mThick, float radius) {
		mParentView = parentView;
		mAlt = alt;
		mThickness = mThick;
		mRadius = radius;
	}

	public void setRadialMenuContent(List<RadialMenuItem> radialMenuContent) {
		mRadialMenuContent = radialMenuContent;
	}

	public RadialMenuView renderView() {
		final RadialMenuView menu = new RadialMenuView(
				mParentView.getContext(), this);
		mParentView.setOnTouchListener(new View.OnTouchListener() {
			public boolean onTouch(View v, MotionEvent event) {
				return menu.gestureHandler(event);
			}
		});
		return menu;
	}

	public List<RadialMenuItem> getRadialMenuContent() {
		return mRadialMenuContent;
	}

	public boolean isAlt() {
		return mAlt;
	}

	public float getMenuThickness() {
		return mThickness;
	}

	public float getRadius() {
		return mRadius;
	}

	public int getMenuBackgroundColor() {
		return mMenuBackgroundColor;
	}

	public void setMenuBackgroundColor(int menuBackgroundColor) {
		mMenuBackgroundColor = menuBackgroundColor;
	}

	public int getMenuSelectedColor() {
		return mMenuSelectedColor;
	}

	public void setMenuSelectedColor(int menuSelectedColor) {
		mMenuSelectedColor = menuSelectedColor;
	}

	public int getMenuTextColor() {
		return mMenuTextColor;
	}

	public void setMenuTextColor(int menuTextColor) {
		mMenuTextColor = menuTextColor;
	}

	public int getMenuBorderColor() {
		return mMenuBorderColor;
	}

	public void setMenuBorderColor(int menuBorderColor) {
		mMenuBorderColor = menuBorderColor;
	}
}