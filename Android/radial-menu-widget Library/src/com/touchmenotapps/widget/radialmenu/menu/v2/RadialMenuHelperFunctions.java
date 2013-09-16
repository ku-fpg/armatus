package com.touchmenotapps.widget.radialmenu.menu.v2;

public class RadialMenuHelperFunctions {
	public float distance(float mWidth, float mHeight, float x2, float y2) {
		double dx = mWidth - x2;
		double dy = mHeight - y2;
		float dist = (float) Math.sqrt(dx * dx + dy * dy);
		return dist;
	}

	public float angle(float mWidth, float mHeight, float x2, float y2,
			boolean alt, int items) {
		double dx = x2 - mWidth;
		double dy = y2 - mHeight;
		float angle = (float) (Math.atan2(dy, dx) * 180.0D / 3.141592653589793D)
				+ 90.0F + (alt ? 360 / items / 2 : 0);
		if (angle < 0.0F)
			return (angle + 360.0F) / (360 / items);
		return angle / (360 / items);
	}
}