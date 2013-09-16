package com.touchmenotapps.widget.radialmenu.menu.v1;

import android.graphics.Path;
import android.graphics.RectF;

public class RadialMenuWedge extends Path {
	private int x;
	private int y;
	private int InnerSize;
	private int OuterSize;
	private float StartArc;
	private float ArcWidth;

	protected RadialMenuWedge(int x, int y, int InnerSize, int OuterSize,
			float StartArc, float ArcWidth) {
		if (StartArc >= 360.0F) {
			StartArc -= 360.0F;
		}
		this.x = x;
		this.y = y;
		this.InnerSize = InnerSize;
		this.OuterSize = OuterSize;
		this.StartArc = StartArc;
		this.ArcWidth = ArcWidth;
		buildPath();
	}

	protected void buildPath() {
		RectF rect = new RectF();
		RectF rect2 = new RectF();

		rect.set(this.x - this.InnerSize, this.y - this.InnerSize, this.x
				+ this.InnerSize, this.y + this.InnerSize);
		rect2.set(this.x - this.OuterSize, this.y - this.OuterSize, this.x
				+ this.OuterSize, this.y + this.OuterSize);
		reset();

		arcTo(rect2, this.StartArc, this.ArcWidth);
		arcTo(rect, this.StartArc + this.ArcWidth, -this.ArcWidth);
		close();
	}
}