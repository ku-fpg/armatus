package com.touchmenotapps.widget.radialmenu.menu.v1;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.drawable.BitmapDrawable;
import android.view.View;
import android.view.animation.AccelerateInterpolator;
import android.view.animation.AnimationSet;
import android.view.animation.DecelerateInterpolator;
import android.view.animation.RotateAnimation;
import android.view.animation.ScaleAnimation;
import android.view.animation.TranslateAnimation;
import android.widget.PopupWindow;

public class RadialMenuHelper {
	private RotateAnimation mRotate;
	private ScaleAnimation mScale;
	private TranslateAnimation mMove;
	private long mAnimationSpeed = 0L;

	protected PopupWindow initPopup(Context context) {
		PopupWindow window = new PopupWindow(context);
		window.setWidth(-2);
		window.setHeight(-2);
		window.setTouchable(true);
		window.setFocusable(true);
		window.setOutsideTouchable(true);
		window.setBackgroundDrawable(new BitmapDrawable(context.getResources(), (Bitmap) null));
		return window;
	}

	protected void onOpenAnimation(View view, int xPosition, int yPosition,
			int xSource, int ySource) {
		this.mRotate = new RotateAnimation(0.0F, 360.0F, xPosition, yPosition);
		this.mScale = new ScaleAnimation(0.0F, 1.0F, 0.0F, 1.0F, xPosition,
				yPosition);
		this.mScale.setInterpolator(new DecelerateInterpolator());
		this.mMove = new TranslateAnimation(xSource - xPosition, 0.0F, ySource
				- yPosition, 0.0F);

		AnimationSet spriteAnimation = new AnimationSet(true);
		spriteAnimation.addAnimation(this.mRotate);
		spriteAnimation.addAnimation(this.mScale);
		spriteAnimation.addAnimation(this.mMove);
		spriteAnimation.setDuration(this.mAnimationSpeed);

		view.startAnimation(spriteAnimation);
	}

	protected void onOpenAnimation(View view, int xPosition, int yPosition,
			int xSource, int ySource, long animTime) {
		this.mRotate = new RotateAnimation(0.0F, 360.0F, xPosition, yPosition);
		this.mScale = new ScaleAnimation(0.0F, 1.0F, 0.0F, 1.0F, xPosition,
				yPosition);
		this.mScale.setInterpolator(new DecelerateInterpolator());
		this.mMove = new TranslateAnimation(xSource - xPosition, 0.0F, ySource
				- yPosition, 0.0F);

		AnimationSet spriteAnimation = new AnimationSet(true);
		spriteAnimation.addAnimation(this.mRotate);
		spriteAnimation.addAnimation(this.mScale);
		spriteAnimation.addAnimation(this.mMove);
		spriteAnimation.setDuration(animTime);

		view.startAnimation(spriteAnimation);
	}

	protected void onCloseAnimation(View view, int xPosition, int yPosition,
			int xSource, int ySource) {
		this.mRotate = new RotateAnimation(360.0F, 0.0F, xPosition, yPosition);
		this.mScale = new ScaleAnimation(1.0F, 0.0F, 1.0F, 0.0F, xPosition,
				yPosition);
		this.mScale.setInterpolator(new AccelerateInterpolator());
		this.mMove = new TranslateAnimation(0.0F, xSource - xPosition, 0.0F,
				ySource - yPosition);

		AnimationSet spriteAnimation = new AnimationSet(true);
		spriteAnimation.addAnimation(this.mRotate);
		spriteAnimation.addAnimation(this.mScale);
		spriteAnimation.addAnimation(this.mMove);
		spriteAnimation.setDuration(this.mAnimationSpeed);

		view.startAnimation(spriteAnimation);
	}

	protected void onCloseAnimation(View view, int xPosition, int yPosition,
			int xSource, int ySource, long animTime) {
		this.mRotate = new RotateAnimation(360.0F, 0.0F, xPosition, yPosition);
		this.mScale = new ScaleAnimation(1.0F, 0.0F, 1.0F, 0.0F, xPosition,
				yPosition);
		this.mScale.setInterpolator(new AccelerateInterpolator());
		this.mMove = new TranslateAnimation(0.0F, xSource - xPosition, 0.0F,
				ySource - yPosition);

		AnimationSet spriteAnimation = new AnimationSet(true);
		spriteAnimation.addAnimation(this.mRotate);
		spriteAnimation.addAnimation(this.mScale);
		spriteAnimation.addAnimation(this.mMove);
		spriteAnimation.setDuration(animTime);

		view.startAnimation(spriteAnimation);
	}

	protected boolean pntInWedge(double px, double py, float xRadiusCenter,
			float yRadiusCenter, int innerRadius, int outerRadius,
			double startAngle, double sweepAngle) {
		double diffX = px - xRadiusCenter;
		double diffY = py - yRadiusCenter;
		double angle = Math.atan2(diffY, diffX);
		if (angle < 0.0D)
			angle += 6.283185307179586D;
		if (startAngle >= 6.283185307179586D) {
			startAngle -= 6.283185307179586D;
		}

		if (((angle >= startAngle) && (angle <= startAngle + sweepAngle))
				|| ((angle + 6.283185307179586D >= startAngle) && (angle + 6.283185307179586D <= startAngle
						+ sweepAngle))) {
			double dist = diffX * diffX + diffY * diffY;
			if ((dist < outerRadius * outerRadius)
					&& (dist > innerRadius * innerRadius)) {
				return true;
			}
		}
		return false;
	}

	protected boolean pntInCircle(double px, double py, double x1, double y1,
			double radius) {
		double diffX = x1 - px;
		double diffY = y1 - py;
		double dist = diffX * diffX + diffY * diffY;
		if (dist < radius * radius) {
			return true;
		}
		return false;
	}
}