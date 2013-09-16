package com.touchmenotapps.widget.radialmenu.menu.v1;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.RectF;
import android.graphics.drawable.Drawable;
import android.view.MotionEvent;
import android.view.View;
import android.widget.PopupWindow;
import java.util.ArrayList;
import java.util.List;

public class RadialMenuWidget extends View {
	private RadialMenuHelper helper;
	private List<RadialMenuItem> menuEntries = new ArrayList<RadialMenuItem>();
	private RadialMenuItem centerCircle = null;

	private float screen_density = getContext().getResources()
			.getDisplayMetrics().density;

	private int defaultColor = Color.rgb(34, 96, 120);

	private int defaultAlpha = 180;

	private int wedge2Color = Color.rgb(50, 50, 50);

	private int wedge2Alpha = 210;
	private int outlineColor = Color.rgb(150, 150, 150);
	private int outlineAlpha = 255;
	private int selectedColor = Color.rgb(70, 130, 180);

	private int selectedAlpha = 210;

	private int disabledColor = Color.rgb(34, 96, 120);

	private int disabledAlpha = 100;

	private int pictureAlpha = 255;

	private int textColor = Color.rgb(255, 255, 255);

	private int textAlpha = 255;

	private int headerTextColor = Color.rgb(255, 255, 255);

	private int headerTextAlpha = 255;
	private int headerBackgroundColor = Color.rgb(0, 0, 0);

	private int headerBackgroundAlpha = 180;

	private int wedgeQty = 1;
	private RadialMenuWedge[] Wedges = new RadialMenuWedge[this.wedgeQty];
	private RadialMenuWedge selected = null;

	private RadialMenuWedge enabled = null;

	private Rect[] iconRect = new Rect[this.wedgeQty];

	private int wedgeQty2 = 1;
	private RadialMenuWedge[] Wedges2 = new RadialMenuWedge[this.wedgeQty2];
	private RadialMenuWedge selected2 = null;

	private Rect[] iconRect2 = new Rect[this.wedgeQty2];
	private RadialMenuInterface wedge2Data = null;

	private int MinSize = scalePX(35);
	private int MaxSize = scalePX(90);
	private int r2MinSize = this.MaxSize + scalePX(5);

	private int r2MaxSize = this.r2MinSize + scalePX(45);

	private int MinIconSize = scalePX(15);
	private int MaxIconSize = scalePX(35);

	private int cRadius = this.MinSize - scalePX(7);
	private int textSize = scalePX(15);
	private int animateTextSize = this.textSize;

	private int xPosition = scalePX(120);
	private int yPosition = scalePX(120);

	private int xSource = 0;
	private int ySource = 0;
	private boolean showSource = false;

	private boolean inWedge = false;

	private boolean inWedge2 = false;

	private boolean inCircle = false;

	private boolean Wedge2Shown = false;
	private boolean HeaderBoxBounded = false;

	private String headerString = null;
	private int headerTextSize = this.textSize;
	private int headerBuffer = scalePX(8);
	private Rect textRect = new Rect();
	private RectF textBoxRect = new RectF();
	private int headerTextLeft;
	private int headerTextBottom;
	//private static final int ANIMATE_IN = 1;
	//private static final int ANIMATE_OUT = 2;
	private int animateSections = 4;
	private int r2VariableSize;
	private boolean animateOuterIn = false;
	private boolean animateOuterOut = false;
	private PopupWindow mWindow;
	
	private Paint mPaint = new Paint();
	private Rect mRect1 = new Rect();
	private Rect mRect2 = new Rect();
	private Rect mRectText = new Rect();
	private Rect mRectIcon = new Rect();

	public RadialMenuWidget(Context context) {
		super(context);
		this.helper = new RadialMenuHelper();
		this.mWindow = this.helper.initPopup(context);

		this.xPosition = (getResources().getDisplayMetrics().widthPixels / 2);
		this.yPosition = (getResources().getDisplayMetrics().heightPixels / 2);

		determineWedges();
		this.helper.onOpenAnimation(this, this.xPosition, this.yPosition,
				this.xSource, this.ySource);
	}

	public boolean onTouchEvent(MotionEvent e) {
		int state = e.getAction();
		int eventX = (int) e.getX();
		int eventY = (int) e.getY();
		if (state == 0) {
			this.inWedge = false;
			this.inWedge2 = false;
			this.inCircle = false;

			for (int i = 0; i < this.Wedges.length; i++) {
				RadialMenuWedge f = this.Wedges[i];
				double slice = 6.283185307179586D / this.wedgeQty;
				double start = 4.71238898038469D - slice / 2.0D;

				this.inWedge = this.helper.pntInWedge(eventX, eventY,
						this.xPosition, this.yPosition, this.MinSize,
						this.MaxSize, i * slice + start, slice);

				if (this.inWedge) {
					this.selected = f;
					break;
				}

			}

			if (this.Wedge2Shown) {
				for (int i = 0; i < this.Wedges2.length; i++) {
					RadialMenuWedge f = this.Wedges2[i];
					double slice = 6.283185307179586D / this.wedgeQty2;
					double start = 4.71238898038469D - slice / 2.0D;

					this.inWedge2 = this.helper.pntInWedge(eventX, eventY,
							this.xPosition, this.yPosition, this.r2MinSize,
							this.r2MaxSize, i * slice + start, slice);

					if (this.inWedge2) {
						this.selected2 = f;
						break;
					}

				}

			}

			if (this.centerCircle != null) {
				this.inCircle = this.helper.pntInCircle(eventX, eventY,
						this.xPosition, this.yPosition, this.cRadius);
			}
		} else if (state == 1) {
			if (this.inCircle) {
				if (this.Wedge2Shown) {
					this.enabled = null;
					this.animateOuterIn = true;
				}
				this.selected = null;
				this.centerCircle.menuActiviated();
			} else if (this.selected != null) {
				for (int i = 0; i < this.Wedges.length; i++) {
					RadialMenuWedge f = this.Wedges[i];
					if (f == this.selected) {
						if (this.enabled != null) {
							this.enabled = null;
							this.animateOuterIn = true;
						} else {
							((RadialMenuItem) this.menuEntries.get(i))
									.menuActiviated();

							if (((RadialMenuItem) this.menuEntries.get(i))
									.getChildren() != null) {
								determineOuterWedges((RadialMenuItem) this.menuEntries
										.get(i));
								this.enabled = f;
								this.animateOuterOut = true;
							} else {
								this.Wedge2Shown = false;
							}
						}
						this.selected = null;
					}
				}
			} else if (this.selected2 != null) {
				for (int i = 0; i < this.Wedges2.length; i++) {
					RadialMenuWedge f = this.Wedges2[i];
					if (f == this.selected2) {
						this.animateOuterIn = true;
						this.enabled = null;
						this.selected = null;
						((RadialMenuItem) this.wedge2Data.getChildren().get(i))
								.menuActiviated();
					}
				}
			} else {
				dismiss();
			}

			this.selected2 = null;
			this.inCircle = false;
		}
		invalidate();
		return true;
	}

	protected void onDraw(Canvas c) {
		mPaint.reset();
		mPaint.setAntiAlias(true);
		mPaint.setStrokeWidth(3.0F);

		if (this.showSource) {
			mPaint.setColor(this.outlineColor);
			mPaint.setAlpha(this.outlineAlpha);
			mPaint.setStyle(Paint.Style.STROKE);
			c.drawCircle(this.xSource, this.ySource, this.cRadius / 10, mPaint);

			mPaint.setColor(this.selectedColor);
			mPaint.setAlpha(this.selectedAlpha);
			mPaint.setStyle(Paint.Style.FILL);
			c.drawCircle(this.xSource, this.ySource, this.cRadius / 10, mPaint);
		}

		for (int i = 0; i < this.Wedges.length; i++) {
			RadialMenuWedge f = this.Wedges[i];
			mPaint.setColor(this.outlineColor);
			mPaint.setAlpha(this.outlineAlpha);
			mPaint.setStyle(Paint.Style.STROKE);
			c.drawPath(f, mPaint);
			if ((f == this.enabled) && (this.Wedge2Shown)) {
				mPaint.setColor(this.wedge2Color);
				mPaint.setAlpha(this.wedge2Alpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			} else if ((f != this.enabled) && (this.Wedge2Shown)) {
				mPaint.setColor(this.disabledColor);
				mPaint.setAlpha(this.disabledAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			} else if ((f == this.enabled) && (!this.Wedge2Shown)) {
				mPaint.setColor(this.wedge2Color);
				mPaint.setAlpha(this.wedge2Alpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			} else if (f == this.selected) {
				mPaint.setColor(this.wedge2Color);
				mPaint.setAlpha(this.wedge2Alpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			} else {
				mPaint.setColor(this.defaultColor);
				mPaint.setAlpha(this.defaultAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			}

			Rect rf = this.iconRect[i];

			if ((((RadialMenuItem) this.menuEntries.get(i)).getIcon() != 0)
					&& (((RadialMenuItem) this.menuEntries.get(i)).getLabel() != null)) {
				String menuItemName = ((RadialMenuItem) this.menuEntries.get(i))
						.getLabel();
				String[] stringArray = menuItemName.split("\n");

				mPaint.setColor(this.textColor);
				if ((f != this.enabled) && (this.Wedge2Shown))
					mPaint.setAlpha(this.disabledAlpha);
				else {
					mPaint.setAlpha(this.textAlpha);
				}
				mPaint.setStyle(Paint.Style.FILL);
				mPaint.setTextSize(this.textSize);

				mRect1.setEmpty();
				float textHeight = 0.0F;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRect1);
					textHeight += mRect1.height() + 3;
				}

				mRect2.setEmpty();
				mRect2.set(rf.left, rf.top - (int) textHeight / 2, rf.right,
						rf.bottom - (int) textHeight / 2);

				float textBottom = mRect2.bottom;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRect1);
					float textLeft = rf.centerX() - mRect1.width() / 2;
					textBottom += mRect1.height() + 3;
					c.drawText(stringArray[j], textLeft - mRect1.left, textBottom
							- mRect1.bottom, mPaint);
				}

				Drawable drawable = getResources().getDrawable(
						((RadialMenuItem) this.menuEntries.get(i)).getIcon());
				drawable.setBounds(mRect2);
				if ((f != this.enabled) && (this.Wedge2Shown))
					drawable.setAlpha(this.disabledAlpha);
				else {
					drawable.setAlpha(this.pictureAlpha);
				}
				drawable.draw(c);
			} else if (((RadialMenuItem) this.menuEntries.get(i)).getIcon() != 0) {
				Drawable drawable = getResources().getDrawable(
						((RadialMenuItem) this.menuEntries.get(i)).getIcon());
				drawable.setBounds(rf);
				if ((f != this.enabled) && (this.Wedge2Shown))
					drawable.setAlpha(this.disabledAlpha);
				else {
					drawable.setAlpha(this.pictureAlpha);
				}
				drawable.draw(c);
			} else {
				mPaint.setColor(this.textColor);
				if ((f != this.enabled) && (this.Wedge2Shown))
					mPaint.setAlpha(this.disabledAlpha);
				else {
					mPaint.setAlpha(this.textAlpha);
				}
				mPaint.setStyle(Paint.Style.FILL);
				mPaint.setTextSize(this.textSize);

				String menuItemName = ((RadialMenuItem) this.menuEntries.get(i))
						.getLabel();
				String[] stringArray = menuItemName.split("\n");

				mRect1.setEmpty();
				float textHeight = 0.0F;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRect1);
					textHeight += mRect1.height() + 3;
				}

				float textBottom = rf.centerY() - textHeight / 2.0F;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRect1);
					float textLeft = rf.centerX() - mRect1.width() / 2;
					textBottom += mRect1.height() + 3;
					c.drawText(stringArray[j], textLeft - mRect1.left, textBottom
							- mRect1.bottom, mPaint);
				}

			}

		}

		if (this.animateOuterIn)
			animateOuterWedges(1);
		else if (this.animateOuterOut) {
			animateOuterWedges(2);
		}

		if (this.Wedge2Shown) {
			for (int i = 0; i < this.Wedges2.length; i++) {
				RadialMenuWedge f = this.Wedges2[i];
				mPaint.setColor(this.outlineColor);
				mPaint.setAlpha(this.outlineAlpha);
				mPaint.setStyle(Paint.Style.STROKE);
				c.drawPath(f, mPaint);
				if (f == this.selected2) {
					mPaint.setColor(this.selectedColor);
					mPaint.setAlpha(this.selectedAlpha);
					mPaint.setStyle(Paint.Style.FILL);
					c.drawPath(f, mPaint);
				} else {
					mPaint.setColor(this.wedge2Color);
					mPaint.setAlpha(this.wedge2Alpha);
					mPaint.setStyle(Paint.Style.FILL);
					c.drawPath(f, mPaint);
				}

				Rect rf = this.iconRect2[i];
				if ((((RadialMenuItem) this.wedge2Data.getChildren().get(i))
						.getIcon() != 0)
						&& (((RadialMenuItem) this.wedge2Data.getChildren()
								.get(i)).getLabel() != null)) {
					String menuItemName = ((RadialMenuItem) this.wedge2Data
							.getChildren().get(i)).getLabel();
					String[] stringArray = menuItemName.split("\n");

					mPaint.setColor(this.textColor);
					mPaint.setAlpha(this.textAlpha);
					mPaint.setStyle(Paint.Style.FILL);
					mPaint.setTextSize(this.animateTextSize);

					mRect1.setEmpty();
					float textHeight = 0.0F;
					for (int j = 0; j < stringArray.length; j++) {
						mPaint.getTextBounds(stringArray[j], 0,
								stringArray[j].length(), mRect1);
						textHeight += mRect1.height() + 3;
					}

					mRect2.setEmpty();
					mRect2.set(rf.left, rf.top - (int) textHeight / 2, rf.right,
							rf.bottom - (int) textHeight / 2);

					float textBottom = mRect2.bottom;
					for (int j = 0; j < stringArray.length; j++) {
						mPaint.getTextBounds(stringArray[j], 0,
								stringArray[j].length(), mRect1);
						float textLeft = rf.centerX() - mRect1.width() / 2;
						textBottom += mRect1.height() + 3;
						c.drawText(stringArray[j], textLeft - mRect1.left,
								textBottom - mRect1.bottom, mPaint);
					}

					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) this.wedge2Data.getChildren()
									.get(i)).getIcon());
					drawable.setBounds(mRect2);
					drawable.setAlpha(this.pictureAlpha);
					drawable.draw(c);
				} else if (((RadialMenuItem) this.wedge2Data.getChildren().get(
						i)).getIcon() != 0) {
					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) this.wedge2Data.getChildren()
									.get(i)).getIcon());
					drawable.setBounds(rf);
					drawable.setAlpha(this.pictureAlpha);
					drawable.draw(c);
				} else {
					mPaint.setColor(this.textColor);
					mPaint.setAlpha(this.textAlpha);
					mPaint.setStyle(Paint.Style.FILL);
					mPaint.setTextSize(this.animateTextSize);

					String menuItemName = ((RadialMenuItem) this.wedge2Data
							.getChildren().get(i)).getLabel();
					String[] stringArray = menuItemName.split("\n");

					mRect1.setEmpty();
					float textHeight = 0.0F;
					for (int j = 0; j < stringArray.length; j++) {
						mPaint.getTextBounds(stringArray[j], 0,
								stringArray[j].length(), mRect1);
						textHeight += mRect1.height() + 3;
					}

					float textBottom = rf.centerY() - textHeight / 2.0F;
					for (int j = 0; j < stringArray.length; j++) {
						mPaint.getTextBounds(stringArray[j], 0,
								stringArray[j].length(), mRect1);
						float textLeft = rf.centerX() - mRect1.width() / 2;
						textBottom += mRect1.height() + 3;
						c.drawText(stringArray[j], textLeft - mRect1.left,
								textBottom - mRect1.bottom, mPaint);
					}
				}
			}

		}

		if (this.centerCircle != null) {
			mPaint.setColor(this.outlineColor);
			mPaint.setAlpha(this.outlineAlpha);
			mPaint.setStyle(Paint.Style.STROKE);
			c.drawCircle(this.xPosition, this.yPosition, this.cRadius, mPaint);
			if (this.inCircle) {
				mPaint.setColor(this.selectedColor);
				mPaint.setAlpha(this.selectedAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawCircle(this.xPosition, this.yPosition, this.cRadius,
						mPaint);
				this.helper.onCloseAnimation(this, this.xPosition,
						this.yPosition, this.xSource, this.ySource);
			} else {
				mPaint.setColor(this.defaultColor);
				mPaint.setAlpha(this.defaultAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawCircle(this.xPosition, this.yPosition, this.cRadius,
						mPaint);
			}

			if ((this.centerCircle.getIcon() != 0)
					&& (this.centerCircle.getLabel() != null)) {
				String menuItemName = this.centerCircle.getLabel();
				String[] stringArray = menuItemName.split("\n");

				mPaint.setColor(this.textColor);
				mPaint.setAlpha(this.textAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				mPaint.setTextSize(this.textSize);

				mRectIcon.setEmpty();
				mRectText.setEmpty();
				Drawable drawable = getResources().getDrawable(
						this.centerCircle.getIcon());

				int h = getIconSize(drawable.getIntrinsicHeight(),
						this.MinIconSize, this.MaxIconSize);
				int w = getIconSize(drawable.getIntrinsicWidth(),
						this.MinIconSize, this.MaxIconSize);
				mRectIcon.set(this.xPosition - w / 2, this.yPosition - h / 2,
						this.xPosition + w / 2, this.yPosition + h / 2);

				float textHeight = 0.0F;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRectText);
					textHeight += mRectText.height() + 3;
				}

				mRectIcon.set(mRectIcon.left,
						mRectIcon.top - (int) textHeight / 2, mRectIcon.right,
						mRectIcon.bottom - (int) textHeight / 2);

				float textBottom = mRectIcon.bottom;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRectText);
					float textLeft = this.xPosition - mRectText.width() / 2;
					textBottom += mRectText.height() + 3;
					c.drawText(stringArray[j], textLeft - mRectText.left,
							textBottom - mRectText.bottom, mPaint);
				}

				drawable.setBounds(mRectIcon);
				drawable.setAlpha(this.pictureAlpha);
				drawable.draw(c);
			} else if (this.centerCircle.getIcon() != 0) {
				mRect1.setEmpty();

				Drawable drawable = getResources().getDrawable(
						this.centerCircle.getIcon());

				int h = getIconSize(drawable.getIntrinsicHeight(),
						this.MinIconSize, this.MaxIconSize);
				int w = getIconSize(drawable.getIntrinsicWidth(),
						this.MinIconSize, this.MaxIconSize);
				mRect1.set(this.xPosition - w / 2, this.yPosition - h / 2,
						this.xPosition + w / 2, this.yPosition + h / 2);

				drawable.setBounds(mRect1);
				drawable.setAlpha(this.pictureAlpha);
				drawable.draw(c);
			} else {
				mPaint.setColor(this.textColor);
				mPaint.setAlpha(this.textAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				mPaint.setTextSize(this.textSize);

				String menuItemName = this.centerCircle.getLabel();
				String[] stringArray = menuItemName.split("\n");

				mRect1.setEmpty();
				float textHeight = 0.0F;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRect1);
					textHeight += mRect1.height() + 3;
				}

				float textBottom = this.yPosition - textHeight / 2.0F;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRect1);
					float textLeft = this.xPosition - mRect1.width() / 2;
					textBottom += mRect1.height() + 3;
					c.drawText(stringArray[j], textLeft - mRect1.left, textBottom
							- mRect1.bottom, mPaint);
				}

			}

		}

		if (this.headerString != null) {
			mPaint.setTextSize(this.headerTextSize);
			mPaint.getTextBounds(this.headerString, 0,
					this.headerString.length(), this.textRect);
			if (!this.HeaderBoxBounded) {
				determineHeaderBox();
				this.HeaderBoxBounded = true;
			}

			mPaint.setColor(this.outlineColor);
			mPaint.setAlpha(this.outlineAlpha);
			mPaint.setStyle(Paint.Style.STROKE);
			c.drawRoundRect(this.textBoxRect, scalePX(5), scalePX(5), mPaint);
			mPaint.setColor(this.headerBackgroundColor);
			mPaint.setAlpha(this.headerBackgroundAlpha);
			mPaint.setStyle(Paint.Style.FILL);
			c.drawRoundRect(this.textBoxRect, scalePX(5), scalePX(5), mPaint);

			mPaint.setColor(this.headerTextColor);
			mPaint.setAlpha(this.headerTextAlpha);
			mPaint.setStyle(Paint.Style.FILL);
			mPaint.setTextSize(this.headerTextSize);
			c.drawText(this.headerString, this.headerTextLeft,
					this.headerTextBottom, mPaint);
		}
	}

	private int scalePX(int dp_size) {
		int px_size = (int) (dp_size * this.screen_density + 0.5F);
		return px_size;
	}

	private int getIconSize(int iconSize, int minSize, int maxSize) {
		if (iconSize > minSize) {
			if (iconSize > maxSize) {
				return maxSize;
			}
			return iconSize;
		}

		return minSize;
	}

	private void animateOuterWedges(int animation_direction) {
		boolean animationComplete = false;

		float slice2 = 360 / this.wedgeQty2;
		float start_slice2 = 270.0F - slice2 / 2.0F;

		double rSlice2 = 6.283185307179586D / this.wedgeQty2;
		double rStart2 = 4.71238898038469D - rSlice2 / 2.0D;

		this.Wedges2 = new RadialMenuWedge[this.wedgeQty2];
		this.iconRect2 = new Rect[this.wedgeQty2];

		this.Wedge2Shown = true;

		int wedgeSizeChange = (this.r2MaxSize - this.r2MinSize)
				/ this.animateSections;

		if (animation_direction == 2) {
			if (this.r2MinSize + this.r2VariableSize + wedgeSizeChange < this.r2MaxSize) {
				this.r2VariableSize += wedgeSizeChange;
			} else {
				this.animateOuterOut = false;
				this.r2VariableSize = (this.r2MaxSize - this.r2MinSize);
				animationComplete = true;
			}

			this.animateTextSize = (this.textSize / this.animateSections * (this.r2VariableSize / wedgeSizeChange));

			for (int i = 0; i < this.Wedges2.length; i++) {
				this.Wedges2[i] = new RadialMenuWedge(this.xPosition,
						this.yPosition, this.r2MinSize, this.r2MinSize
								+ this.r2VariableSize, i * slice2
								+ start_slice2, slice2);
				float xCenter = (float) (Math.cos(rSlice2 * i + rSlice2 * 0.5D
						+ rStart2)
						* (this.r2MinSize + this.r2VariableSize + this.r2MinSize) / 2.0D)
						+ this.xPosition;
				float yCenter = (float) (Math.sin(rSlice2 * i + rSlice2 * 0.5D
						+ rStart2)
						* (this.r2MinSize + this.r2VariableSize + this.r2MinSize) / 2.0D)
						+ this.yPosition;

				int h = this.MaxIconSize;
				int w = this.MaxIconSize;
				if (((RadialMenuItem) this.wedge2Data.getChildren().get(i))
						.getIcon() != 0) {
					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) this.wedge2Data.getChildren()
									.get(i)).getIcon());
					h = getIconSize(drawable.getIntrinsicHeight(),
							this.MinIconSize, this.MaxIconSize);
					w = getIconSize(drawable.getIntrinsicWidth(),
							this.MinIconSize, this.MaxIconSize);
				}

				if (this.r2VariableSize < h) {
					h = this.r2VariableSize;
				}
				if (this.r2VariableSize < w) {
					w = this.r2VariableSize;
				}

				this.iconRect2[i] = new Rect((int) xCenter - w / 2,
						(int) yCenter - h / 2, (int) xCenter + w / 2,
						(int) yCenter + h / 2);

				int widthOffset = this.MaxSize;
				if (widthOffset < this.textRect.width() / 2) {
					widthOffset = this.textRect.width() / 2 + scalePX(3);
				}
				this.textBoxRect.set(this.xPosition - widthOffset,
						this.yPosition - (this.r2MinSize + this.r2VariableSize)
								- this.headerBuffer - this.textRect.height()
								- scalePX(3), this.xPosition + widthOffset,
						this.yPosition - (this.r2MinSize + this.r2VariableSize)
								- this.headerBuffer + scalePX(3));
				this.headerTextBottom = (this.yPosition
						- (this.r2MinSize + this.r2VariableSize)
						- this.headerBuffer - this.textRect.bottom);
			}

		} else if (animation_direction == 1) {
			if (this.r2MinSize < this.r2MaxSize - this.r2VariableSize
					- wedgeSizeChange) {
				this.r2VariableSize += wedgeSizeChange;
			} else {
				this.animateOuterIn = false;
				this.r2VariableSize = this.r2MaxSize;
				animationComplete = true;
			}

			this.animateTextSize = (this.textSize - this.textSize
					/ this.animateSections
					* (this.r2VariableSize / wedgeSizeChange));

			for (int i = 0; i < this.Wedges2.length; i++) {
				this.Wedges2[i] = new RadialMenuWedge(this.xPosition,
						this.yPosition, this.r2MinSize, this.r2MaxSize
								- this.r2VariableSize, i * slice2
								+ start_slice2, slice2);

				float xCenter = (float) (Math.cos(rSlice2 * i + rSlice2 * 0.5D
						+ rStart2)
						* (this.r2MaxSize - this.r2VariableSize + this.r2MinSize) / 2.0D)
						+ this.xPosition;
				float yCenter = (float) (Math.sin(rSlice2 * i + rSlice2 * 0.5D
						+ rStart2)
						* (this.r2MaxSize - this.r2VariableSize + this.r2MinSize) / 2.0D)
						+ this.yPosition;

				int h = this.MaxIconSize;
				int w = this.MaxIconSize;
				if (((RadialMenuItem) this.wedge2Data.getChildren().get(i))
						.getIcon() != 0) {
					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) this.wedge2Data.getChildren()
									.get(i)).getIcon());
					h = getIconSize(drawable.getIntrinsicHeight(),
							this.MinIconSize, this.MaxIconSize);
					w = getIconSize(drawable.getIntrinsicWidth(),
							this.MinIconSize, this.MaxIconSize);
				}

				if (this.r2MaxSize - this.r2MinSize - this.r2VariableSize < h) {
					h = this.r2MaxSize - this.r2MinSize - this.r2VariableSize;
				}
				if (this.r2MaxSize - this.r2MinSize - this.r2VariableSize < w) {
					w = this.r2MaxSize - this.r2MinSize - this.r2VariableSize;
				}

				this.iconRect2[i] = new Rect((int) xCenter - w / 2,
						(int) yCenter - h / 2, (int) xCenter + w / 2,
						(int) yCenter + h / 2);

				int heightOffset = this.r2MaxSize - this.r2VariableSize;
				int widthOffset = this.MaxSize;
				if (this.MaxSize > this.r2MaxSize - this.r2VariableSize) {
					heightOffset = this.MaxSize;
				}
				if (widthOffset < this.textRect.width() / 2) {
					widthOffset = this.textRect.width() / 2 + scalePX(3);
				}
				this.textBoxRect
						.set(this.xPosition - widthOffset, this.yPosition
								- heightOffset - this.headerBuffer
								- this.textRect.height() - scalePX(3),
								this.xPosition + widthOffset, this.yPosition
										- heightOffset - this.headerBuffer
										+ scalePX(3));
				this.headerTextBottom = (this.yPosition - heightOffset
						- this.headerBuffer - this.textRect.bottom);
			}

		}

		if (animationComplete) {
			this.r2VariableSize = 0;
			this.animateTextSize = this.textSize;
			if (animation_direction == 1) {
				this.Wedge2Shown = false;
			}
		}

		invalidate();
	}

	private void determineWedges() {
		int entriesQty = this.menuEntries.size();
		if (entriesQty > 0) {
			this.wedgeQty = entriesQty;

			float degSlice = 360 / this.wedgeQty;
			float start_degSlice = 270.0F - degSlice / 2.0F;

			double rSlice = 6.283185307179586D / this.wedgeQty;
			double rStart = 4.71238898038469D - rSlice / 2.0D;

			this.Wedges = new RadialMenuWedge[this.wedgeQty];
			this.iconRect = new Rect[this.wedgeQty];

			for (int i = 0; i < this.Wedges.length; i++) {
				this.Wedges[i] = new RadialMenuWedge(this.xPosition,
						this.yPosition, this.MinSize, this.MaxSize, i
								* degSlice + start_degSlice, degSlice);
				float xCenter = (float) (Math.cos(rSlice * i + rSlice * 0.5D
						+ rStart)
						* (this.MaxSize + this.MinSize) / 2.0D)
						+ this.xPosition;
				float yCenter = (float) (Math.sin(rSlice * i + rSlice * 0.5D
						+ rStart)
						* (this.MaxSize + this.MinSize) / 2.0D)
						+ this.yPosition;

				int h = this.MaxIconSize;
				int w = this.MaxIconSize;
				if (((RadialMenuItem) this.menuEntries.get(i)).getIcon() != 0) {
					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) this.menuEntries.get(i))
									.getIcon());
					h = getIconSize(drawable.getIntrinsicHeight(),
							this.MinIconSize, this.MaxIconSize);
					w = getIconSize(drawable.getIntrinsicWidth(),
							this.MinIconSize, this.MaxIconSize);
				}

				this.iconRect[i] = new Rect((int) xCenter - w / 2,
						(int) yCenter - h / 2, (int) xCenter + w / 2,
						(int) yCenter + h / 2);
			}

			invalidate();
		}
	}

	private void determineOuterWedges(RadialMenuItem entry) {
		int entriesQty = entry.getChildren().size();
		this.wedgeQty2 = entriesQty;

		float degSlice2 = 360 / this.wedgeQty2;
		float start_degSlice2 = 270.0F - degSlice2 / 2.0F;

		double rSlice2 = 6.283185307179586D / this.wedgeQty2;
		double rStart2 = 4.71238898038469D - rSlice2 / 2.0D;

		this.Wedges2 = new RadialMenuWedge[this.wedgeQty2];
		this.iconRect2 = new Rect[this.wedgeQty2];

		for (int i = 0; i < this.Wedges2.length; i++) {
			this.Wedges2[i] = new RadialMenuWedge(this.xPosition,
					this.yPosition, this.r2MinSize, this.r2MaxSize, i
							* degSlice2 + start_degSlice2, degSlice2);
			float xCenter = (float) (Math.cos(rSlice2 * i + rSlice2 * 0.5D
					+ rStart2)
					* (this.r2MaxSize + this.r2MinSize) / 2.0D)
					+ this.xPosition;
			float yCenter = (float) (Math.sin(rSlice2 * i + rSlice2 * 0.5D
					+ rStart2)
					* (this.r2MaxSize + this.r2MinSize) / 2.0D)
					+ this.yPosition;

			int h = this.MaxIconSize;
			int w = this.MaxIconSize;
			if (((RadialMenuItem) entry.getChildren().get(i)).getIcon() != 0) {
				Drawable drawable = getResources()
						.getDrawable(
								((RadialMenuItem) entry.getChildren().get(i))
										.getIcon());
				h = getIconSize(drawable.getIntrinsicHeight(),
						this.MinIconSize, this.MaxIconSize);
				w = getIconSize(drawable.getIntrinsicWidth(), this.MinIconSize,
						this.MaxIconSize);
			}
			this.iconRect2[i] = new Rect((int) xCenter - w / 2, (int) yCenter
					- h / 2, (int) xCenter + w / 2, (int) yCenter + h / 2);
		}
		this.wedge2Data = entry;
		invalidate();
	}

	private void determineHeaderBox() {
		this.headerTextLeft = (this.xPosition - this.textRect.width() / 2);
		this.headerTextBottom = (this.yPosition - this.MaxSize
				- this.headerBuffer - this.textRect.bottom);
		int offset = this.MaxSize;
		if (offset < this.textRect.width() / 2) {
			offset = this.textRect.width() / 2 + scalePX(3);
		}
		this.textBoxRect.set(this.xPosition - offset, this.yPosition
				- this.MaxSize - this.headerBuffer - this.textRect.height()
				- scalePX(3), this.xPosition + offset, this.yPosition
				- this.MaxSize - this.headerBuffer + scalePX(3));
	}

	public void addMenuEntry(List<RadialMenuItem> menuItems) {
		this.menuEntries.addAll(menuItems);
		determineWedges();
	}

	public void addMenuEntry(RadialMenuItem menuItem) {
		this.menuEntries.add(menuItem);
		determineWedges();
	}

	public void setCenterCircle(RadialMenuItem menuItem) {
		this.centerCircle = menuItem;
	}

	public void setInnerRingRadius(int InnerRadius, int OuterRadius) {
		this.MinSize = scalePX(InnerRadius);
		this.MaxSize = scalePX(OuterRadius);
		determineWedges();
	}

	public void setOuterRingRadius(int InnerRadius, int OuterRadius) {
		this.r2MinSize = scalePX(InnerRadius);
		this.r2MaxSize = scalePX(OuterRadius);
		determineWedges();
	}

	public void setCenterCircleRadius(int centerRadius) {
		this.cRadius = scalePX(centerRadius);
		determineWedges();
	}

	public void setTextSize(int TextSize) {
		this.textSize = scalePX(TextSize);
		this.animateTextSize = this.textSize;
	}

	public void setIconSize(int minIconSize, int maxIconSize) {
		this.MinIconSize = scalePX(minIconSize);
		this.MaxIconSize = scalePX(maxIconSize);
		determineWedges();
	}

	public void setCenterLocation(int x, int y) {
		this.xPosition = x;
		this.yPosition = y;
		determineWedges();
		this.helper.onOpenAnimation(this, this.xPosition, this.yPosition,
				this.xSource, this.ySource);
	}

	public void setSourceLocation(int x, int y) {
		this.xSource = x;
		this.ySource = y;
		this.helper.onOpenAnimation(this, this.xPosition, this.yPosition,
				this.xSource, this.ySource);
	}

	/** @deprecated */
	public void setShowSourceLocation(boolean showSourceLocation) {
		this.showSource = showSourceLocation;
		this.helper.onOpenAnimation(this, this.xPosition, this.yPosition,
				this.xSource, this.ySource);
	}

	public void setAnimationSpeed(long millis) {
		this.helper.onOpenAnimation(this, this.xPosition, this.yPosition,
				this.xSource, this.ySource, millis);
	}

	public void setInnerRingColor(int color, int alpha) {
		this.defaultColor = color;
		this.defaultAlpha = alpha;
	}

	public void setOuterRingColor(int color, int alpha) {
		this.wedge2Color = color;
		this.wedge2Alpha = alpha;
	}

	public void setOutlineColor(int color, int alpha) {
		this.outlineColor = color;
		this.outlineAlpha = alpha;
	}

	public void setSelectedColor(int color, int alpha) {
		this.selectedColor = color;
		this.selectedAlpha = alpha;
	}

	public void setDisabledColor(int color, int alpha) {
		this.disabledColor = color;
		this.disabledAlpha = alpha;
	}

	public void setTextColor(int color, int alpha) {
		this.textColor = color;
		this.textAlpha = alpha;
	}

	public void setHeader(String header, int TextSize) {
		this.headerString = header;
		this.headerTextSize = scalePX(TextSize);
		this.HeaderBoxBounded = false;
	}

	public void setHeaderColors(int TextColor, int TextAlpha, int BgColor,
			int BgAlpha) {
		this.headerTextColor = TextColor;
		this.headerTextAlpha = TextAlpha;
		this.headerBackgroundColor = BgColor;
		this.headerBackgroundAlpha = BgAlpha;
	}

	public void show(View anchor, int posX, int posY) {
		this.mWindow.setContentView(this);
		this.mWindow.showAtLocation(anchor, 0, posX, posY);
	}

	public void show(View anchor) {
		this.mWindow.setContentView(this);
		this.mWindow.showAtLocation(anchor, 0, this.xSource, this.ySource);
	}

	public void dismiss() {
		if (this.mWindow != null)
			this.mWindow.dismiss();
	}
}