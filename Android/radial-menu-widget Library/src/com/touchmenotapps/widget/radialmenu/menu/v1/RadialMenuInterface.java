package com.touchmenotapps.widget.radialmenu.menu.v1;

import java.util.List;

public abstract interface RadialMenuInterface {
	public abstract String getName();

	public abstract String getLabel();

	public abstract int getIcon();

	public abstract List<RadialMenuItem> getChildren();

	public abstract void menuActiviated();
}