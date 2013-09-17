package com.touchmenotapps.widget.radialmenu.menu.v2;

public class RadialMenuItem {
	private String mMenuId;
	private String mMenuName;
	private OnClickListener mCallback;

	public RadialMenuItem(String menuId, String menuName) {
		mMenuId = menuId;
		mMenuName = menuName;
	}

	public String getMenuId() {
		return mMenuId;
	}

	public String getMenuName() {
		return mMenuName;
	}

	public void setOnClickListener(OnClickListener listener) {
		mCallback = listener;
	}

	public OnClickListener getOnClickListener() {
		return mCallback;
	}
	
	public static abstract interface OnClickListener {
		public abstract void onClick(String menuId, String menuName);
	}
}