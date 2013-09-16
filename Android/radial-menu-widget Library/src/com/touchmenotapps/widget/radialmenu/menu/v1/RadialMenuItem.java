package com.touchmenotapps.widget.radialmenu.menu.v1;

import android.util.Log;
import java.util.List;

public class RadialMenuItem implements RadialMenuInterface {
	private String menuName = "Empty";
	private String menuLabel = null;
	private int menuIcon = 0;
	private List<RadialMenuItem> menuChildren = null;
	private RadialMenuItemClickListener menuListener = null;

	public RadialMenuItem(String name, String displayName) {
		if (name != null)
			this.menuName = name;
		this.menuLabel = displayName;
	}

	public void setDisplayIcon(int displayIcon) {
		this.menuIcon = displayIcon;
	}

	public void setOnMenuItemPressed(RadialMenuItemClickListener listener) {
		this.menuListener = listener;
	}

	public void setMenuChildren(List<RadialMenuItem> childItems) {
		this.menuChildren = childItems;
	}

	public String getName() {
		return this.menuName;
	}

	public String getLabel() {
		return this.menuLabel;
	}

	public int getIcon() {
		return this.menuIcon;
	}

	public List<RadialMenuItem> getChildren() {
		return this.menuChildren;
	}

	public void menuActiviated() {
		Log.i(getClass().getName(), this.menuName + " menu pressed.");
		if (this.menuListener != null)
			this.menuListener.execute();
	}

	public static abstract interface RadialMenuItemClickListener {
		public abstract void execute();
	}
}