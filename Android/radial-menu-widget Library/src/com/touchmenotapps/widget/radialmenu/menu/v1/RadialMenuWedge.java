package com.touchmenotapps.widget.radialmenu.menu.v1;

import android.graphics.Path;
import android.graphics.RectF;

public class RadialMenuWedge extends Path {
	private int mX;
	private int mY;
	private int mInnerSize;
	private int mOuterSize;
	private float mStartArc;
	private float mArcWidth;

	protected RadialMenuWedge(int x, int y, int innerSize, int outerSize,
			float startArc, float arcWidth) {
		if (startArc >= 360.0F) {
			startArc -= 360.0F;
		}
		mX = x;
		mY = y;
		mInnerSize = innerSize;
		mOuterSize = outerSize;
		mStartArc = startArc;
		mArcWidth = arcWidth;
		buildPath();
	}

	protected void buildPath() {
		RectF rect = new RectF();
		RectF rect2 = new RectF();

		rect.set(mX - mInnerSize, mY - mInnerSize, mX
				+ mInnerSize, mY + mInnerSize);
		rect2.set(mX - mOuterSize, mY - mOuterSize, mX
				+ mOuterSize, mY + mOuterSize);
		reset();

		arcTo(rect2, mStartArc, mArcWidth);
		arcTo(rect, mStartArc + mArcWidth, -mArcWidth);
		close();
	}
}