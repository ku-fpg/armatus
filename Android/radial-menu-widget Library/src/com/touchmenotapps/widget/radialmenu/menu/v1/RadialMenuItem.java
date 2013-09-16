package com.touchmenotapps.widget.radialmenu.menu.v1;

import android.util.Log;
import java.util.List;

public class RadialMenuItem implements RadialMenuInterface {
	private String mMenuName = "Empty";
	private String mMenuLabel = null;
	private int mMenuIcon = 0;
	private List<RadialMenuItem> mMenuChildren = null;
	private OnClickListener mListener = null;

	public RadialMenuItem(String name, String displayName) {
		if (name != null)
			mMenuName = name;
		mMenuLabel = displayName;
	}

	public void setDisplayIcon(int displayIcon) {
		mMenuIcon = displayIcon;
	}

	public void setOnClickListener(OnClickListener listener) {
		mListener = listener;
	}

	public void setMenuChildren(List<RadialMenuItem> childItems) {
		mMenuChildren = childItems;
	}

	public String getName() {
		return mMenuName;
	}

	public String getLabel() {
		return mMenuLabel;
	}

	public int getIcon() {
		return mMenuIcon;
	}

	public List<RadialMenuItem> getChildren() {
		return mMenuChildren;
	}

	public void menuActiviated() {
		Log.i(getClass().getName(), mMenuName + " menu pressed.");
		if (mListener != null)
			mListener.onClick();
	}

	public static abstract interface OnClickListener {
		public abstract void onClick();
	}
}