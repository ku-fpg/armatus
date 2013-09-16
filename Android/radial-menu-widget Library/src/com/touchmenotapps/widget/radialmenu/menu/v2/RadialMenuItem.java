package com.touchmenotapps.widget.radialmenu.menu.v2;

public class RadialMenuItem {
	private String mMenuID;
	private String mMenuName;
	private OnClickListener mCallback;

	public RadialMenuItem(String menuID, String menuName) {
		mMenuID = menuID;
		mMenuName = menuName;
	}

	public String getMenuID() {
		return mMenuID;
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
		public abstract void onClick(String paramString);
	}
}