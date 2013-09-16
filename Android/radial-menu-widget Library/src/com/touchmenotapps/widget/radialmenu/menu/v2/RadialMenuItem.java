package com.touchmenotapps.widget.radialmenu.menu.v2;

public class RadialMenuItem {
	private String mMenuID;
	private String mMenuName;
	private RadialMenuRenderer.OnRadialMenuClickListener mCallback;

	public RadialMenuItem(String mMenuID, String mMenuName) {
		this.mMenuID = mMenuID;
		this.mMenuName = mMenuName;
	}

	public String getMenuID() {
		return this.mMenuID;
	}

	public String getMenuName() {
		return this.mMenuName;
	}

	public void setOnRadialMenuClickListener(
			RadialMenuRenderer.OnRadialMenuClickListener onRadailMenuClick) {
		this.mCallback = onRadailMenuClick;
	}

	public RadialMenuRenderer.OnRadialMenuClickListener getOnRadailMenuClick() {
		return this.mCallback;
	}
}