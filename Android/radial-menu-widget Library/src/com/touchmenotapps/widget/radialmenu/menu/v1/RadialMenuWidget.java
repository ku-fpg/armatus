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
	private RadialMenuHelper mHelper;
	private List<RadialMenuItem> mMenuEntries = new ArrayList<RadialMenuItem>();
	private RadialMenuItem mCenterCircle = null;
	private float mScreenDensity = getContext().getResources()
			.getDisplayMetrics().density;
	private int mDefaultColor = Color.rgb(34, 96, 120);
	private int mDefaultAlpha = 180;
	private int mWedge2Color = Color.rgb(50, 50, 50);
	private int mWedge2Alpha = 210;
	private int mOutlineColor = Color.rgb(150, 150, 150);
	private int mOutlineAlpha = 255;
	private int mSelectedColor = Color.rgb(70, 130, 180);
	private int mSelectedAlpha = 210;
	private int mDisabledColor = Color.rgb(34, 96, 120);
	private int mDisabledAlpha = 100;
	private int mPictureAlpha = 255;
	private int mTextColor = Color.rgb(255, 255, 255);
	private int mTextAlpha = 255;
	private int mHeaderTextColor = Color.rgb(255, 255, 255);
	private int mHeaderTextAlpha = 255;
	private int mHeaderBackgroundColor = Color.rgb(0, 0, 0);
	private int mHeaderBackgroundAlpha = 180;
	private int mWedgeQty = 1;
	private RadialMenuWedge[] mWedges = new RadialMenuWedge[mWedgeQty];
	private RadialMenuWedge mSelected = null;
	private RadialMenuWedge mEnabled = null;
	private Rect[] mIconRect = new Rect[mWedgeQty];
	private int mWedgeQty2 = 1;
	private RadialMenuWedge[] mWedges2 = new RadialMenuWedge[mWedgeQty2];
	private RadialMenuWedge mSelected2 = null;
	private Rect[] mIconRect2 = new Rect[mWedgeQty2];
	private RadialMenuInterface mWedge2Data = null;
	private int mMinSize = scalePx(35);
	private int mMaxSize = scalePx(90);
	private int mR2MinSize = mMaxSize + scalePx(5);
	private int mR2MaxSize = mR2MinSize + scalePx(45);
	private int mMinIconSize = scalePx(15);
	private int mMaxIconSize = scalePx(35);
	private int mCRadius = mMinSize - scalePx(7);
	private int mTextSize = scalePx(15);
	private int mAnimateTextSize = mTextSize;
	private int mXPosition = scalePx(120);
	private int mYPosition = scalePx(120);
	private int mXSource = 0;
	private int mYSource = 0;
	private boolean mShowSource = false;
	private boolean mInWedge = false;
	private boolean mInWedge2 = false;
	private boolean mInCircle = false;
	private boolean mWedge2Shown = false;
	private boolean mHeaderBoxBounded = false;
	private String mHeaderString = null;
	private int mHeaderTextSize = mTextSize;
	private int mHeaderBuffer = scalePx(8);
	private Rect mTextRect = new Rect();
	private RectF mTextBoxRect = new RectF();
	private int mHeaderTextLeft;
	private int mHeaderTextBottom;
	//private static final int ANIMATE_IN = 1;
	//private static final int ANIMATE_OUT = 2;
	private int mAnimateSections = 4;
	private int mR2VariableSize;
	private boolean mAnimateOuterIn = false;
	private boolean mAnimateOuterOut = false;
	private PopupWindow mWindow;
	private Paint mPaint = new Paint();
	private Rect mRect1 = new Rect();
	private Rect mRect2 = new Rect();
	private Rect mRectText = new Rect();
	private Rect mRectIcon = new Rect();

	public RadialMenuWidget(Context context) {
		super(context);
		mHelper = new RadialMenuHelper();
		mWindow = mHelper.initPopup(context);

		mXPosition = (getResources().getDisplayMetrics().widthPixels / 2);
		mYPosition = (getResources().getDisplayMetrics().heightPixels / 2);

		determineWedges();
		mHelper.onOpenAnimation(this, mXPosition, mYPosition,
				mXSource, mYSource);
	}

	public boolean onTouchEvent(MotionEvent e) {
		int eventX = (int) e.getX();
		int eventY = (int) e.getY();

		switch (e.getAction()) {
		case MotionEvent.ACTION_DOWN:
			mInWedge = false;
			mInWedge2 = false;
			mInCircle = false;

			for (int i = 0; i < mWedges.length; i++) {
				RadialMenuWedge f = mWedges[i];
				double slice = 6.283185307179586D / mWedgeQty;
				double start = 4.71238898038469D - slice / 2.0D;

				mInWedge = mHelper.pntInWedge(eventX, eventY,
						mXPosition, mYPosition, mMinSize,
						mMaxSize, i * slice + start, slice);

				if (mInWedge) {
					mSelected = f;
					break;
				}

			}

			if (mWedge2Shown) {
				for (int i = 0; i < mWedges2.length; i++) {
					RadialMenuWedge f = mWedges2[i];
					double slice = 6.283185307179586D / mWedgeQty2;
					double start = 4.71238898038469D - slice / 2.0D;

					mInWedge2 = mHelper.pntInWedge(eventX, eventY,
							mXPosition, mYPosition, mR2MinSize,
							mR2MaxSize, i * slice + start, slice);

					if (mInWedge2) {
						mSelected2 = f;
						break;
					}

				}

			}

			if (mCenterCircle != null) {
				mInCircle = mHelper.pntInCircle(eventX, eventY,
						mXPosition, mYPosition, mCRadius);
			}
			break;
		case MotionEvent.ACTION_UP:
			if (mInCircle) {
				if (mWedge2Shown) {
					mEnabled = null;
					mAnimateOuterIn = true;
				}
				mSelected = null;
				mCenterCircle.menuActiviated();
			} else if (mSelected != null) {
				for (int i = 0; i < mWedges.length; i++) {
					RadialMenuWedge f = mWedges[i];
					if (f == mSelected) {
						if (mEnabled != null) {
							mEnabled = null;
							mAnimateOuterIn = true;
						} else {
							((RadialMenuItem) mMenuEntries.get(i))
							.menuActiviated();

							if (((RadialMenuItem) mMenuEntries.get(i))
									.getChildren() != null) {
								determineOuterWedges((RadialMenuItem) mMenuEntries
										.get(i));
								mEnabled = f;
								mAnimateOuterOut = true;
							} else {
								mWedge2Shown = false;
							}
						}
						mSelected = null;
					}
				}
			} else if (mSelected2 != null) {
				for (int i = 0; i < mWedges2.length; i++) {
					RadialMenuWedge f = mWedges2[i];
					if (f == mSelected2) {
						mAnimateOuterIn = true;
						mEnabled = null;
						mSelected = null;
						((RadialMenuItem) mWedge2Data.getChildren().get(i))
						.menuActiviated();
					}
				}
			} else {
				dismiss();
			}

			mSelected2 = null;
			mInCircle = false;
			break;
		}
		invalidate();
		return true;
	}

	protected void onDraw(Canvas c) {
		mPaint.reset();
		mPaint.setAntiAlias(true);
		mPaint.setStrokeWidth(3.0F);

		if (mShowSource) {
			mPaint.setColor(mOutlineColor);
			mPaint.setAlpha(mOutlineAlpha);
			mPaint.setStyle(Paint.Style.STROKE);
			c.drawCircle(mXSource, mYSource, mCRadius / 10, mPaint);

			mPaint.setColor(mSelectedColor);
			mPaint.setAlpha(mSelectedAlpha);
			mPaint.setStyle(Paint.Style.FILL);
			c.drawCircle(mXSource, mYSource, mCRadius / 10, mPaint);
		}

		for (int i = 0; i < mWedges.length; i++) {
			RadialMenuWedge f = mWedges[i];
			mPaint.setColor(mOutlineColor);
			mPaint.setAlpha(mOutlineAlpha);
			mPaint.setStyle(Paint.Style.STROKE);
			c.drawPath(f, mPaint);
			if ((f == mEnabled) && (mWedge2Shown)) {
				mPaint.setColor(mWedge2Color);
				mPaint.setAlpha(mWedge2Alpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			} else if ((f != mEnabled) && (mWedge2Shown)) {
				mPaint.setColor(mDisabledColor);
				mPaint.setAlpha(mDisabledAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			} else if ((f == mEnabled) && (!mWedge2Shown)) {
				mPaint.setColor(mWedge2Color);
				mPaint.setAlpha(mWedge2Alpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			} else if (f == mSelected) {
				mPaint.setColor(mWedge2Color);
				mPaint.setAlpha(mWedge2Alpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			} else {
				mPaint.setColor(mDefaultColor);
				mPaint.setAlpha(mDefaultAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawPath(f, mPaint);
			}

			Rect rf = mIconRect[i];

			if ((((RadialMenuItem) mMenuEntries.get(i)).getIcon() != 0)
					&& (((RadialMenuItem) mMenuEntries.get(i)).getLabel() != null)) {
				String menuItemName = ((RadialMenuItem) mMenuEntries.get(i))
						.getLabel();
				String[] stringArray = menuItemName.split("\n");

				mPaint.setColor(mTextColor);
				if ((f != mEnabled) && (mWedge2Shown))
					mPaint.setAlpha(mDisabledAlpha);
				else {
					mPaint.setAlpha(mTextAlpha);
				}
				mPaint.setStyle(Paint.Style.FILL);
				mPaint.setTextSize(mTextSize);

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
						((RadialMenuItem) mMenuEntries.get(i)).getIcon());
				drawable.setBounds(mRect2);
				if ((f != mEnabled) && (mWedge2Shown))
					drawable.setAlpha(mDisabledAlpha);
				else {
					drawable.setAlpha(mPictureAlpha);
				}
				drawable.draw(c);
			} else if (((RadialMenuItem) mMenuEntries.get(i)).getIcon() != 0) {
				Drawable drawable = getResources().getDrawable(
						((RadialMenuItem) mMenuEntries.get(i)).getIcon());
				drawable.setBounds(rf);
				if ((f != mEnabled) && (mWedge2Shown))
					drawable.setAlpha(mDisabledAlpha);
				else {
					drawable.setAlpha(mPictureAlpha);
				}
				drawable.draw(c);
			} else {
				mPaint.setColor(mTextColor);
				if ((f != mEnabled) && (mWedge2Shown))
					mPaint.setAlpha(mDisabledAlpha);
				else {
					mPaint.setAlpha(mTextAlpha);
				}
				mPaint.setStyle(Paint.Style.FILL);
				mPaint.setTextSize(mTextSize);

				String menuItemName = ((RadialMenuItem) mMenuEntries.get(i))
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

		if (mAnimateOuterIn)
			animateOuterWedges(1);
		else if (mAnimateOuterOut) {
			animateOuterWedges(2);
		}

		if (mWedge2Shown) {
			for (int i = 0; i < mWedges2.length; i++) {
				RadialMenuWedge f = mWedges2[i];
				mPaint.setColor(mOutlineColor);
				mPaint.setAlpha(mOutlineAlpha);
				mPaint.setStyle(Paint.Style.STROKE);
				c.drawPath(f, mPaint);
				if (f == mSelected2) {
					mPaint.setColor(mSelectedColor);
					mPaint.setAlpha(mSelectedAlpha);
					mPaint.setStyle(Paint.Style.FILL);
					c.drawPath(f, mPaint);
				} else {
					mPaint.setColor(mWedge2Color);
					mPaint.setAlpha(mWedge2Alpha);
					mPaint.setStyle(Paint.Style.FILL);
					c.drawPath(f, mPaint);
				}

				Rect rf = mIconRect2[i];
				if ((((RadialMenuItem) mWedge2Data.getChildren().get(i))
						.getIcon() != 0)
						&& (((RadialMenuItem) mWedge2Data.getChildren()
								.get(i)).getLabel() != null)) {
					String menuItemName = ((RadialMenuItem) mWedge2Data
							.getChildren().get(i)).getLabel();
					String[] stringArray = menuItemName.split("\n");

					mPaint.setColor(mTextColor);
					mPaint.setAlpha(mTextAlpha);
					mPaint.setStyle(Paint.Style.FILL);
					mPaint.setTextSize(mAnimateTextSize);

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
							((RadialMenuItem) mWedge2Data.getChildren()
									.get(i)).getIcon());
					drawable.setBounds(mRect2);
					drawable.setAlpha(mPictureAlpha);
					drawable.draw(c);
				} else if (((RadialMenuItem) mWedge2Data.getChildren().get(
						i)).getIcon() != 0) {
					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) mWedge2Data.getChildren()
									.get(i)).getIcon());
					drawable.setBounds(rf);
					drawable.setAlpha(mPictureAlpha);
					drawable.draw(c);
				} else {
					mPaint.setColor(mTextColor);
					mPaint.setAlpha(mTextAlpha);
					mPaint.setStyle(Paint.Style.FILL);
					mPaint.setTextSize(mAnimateTextSize);

					String menuItemName = ((RadialMenuItem) mWedge2Data
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

		if (mCenterCircle != null) {
			mPaint.setColor(mOutlineColor);
			mPaint.setAlpha(mOutlineAlpha);
			mPaint.setStyle(Paint.Style.STROKE);
			c.drawCircle(mXPosition, mYPosition, mCRadius, mPaint);
			if (mInCircle) {
				mPaint.setColor(mSelectedColor);
				mPaint.setAlpha(mSelectedAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawCircle(mXPosition, mYPosition, mCRadius,
						mPaint);
				mHelper.onCloseAnimation(this, mXPosition,
						mYPosition, mXSource, mYSource);
			} else {
				mPaint.setColor(mDefaultColor);
				mPaint.setAlpha(mDefaultAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				c.drawCircle(mXPosition, mYPosition, mCRadius,
						mPaint);
			}

			if ((mCenterCircle.getIcon() != 0)
					&& (mCenterCircle.getLabel() != null)) {
				String menuItemName = mCenterCircle.getLabel();
				String[] stringArray = menuItemName.split("\n");

				mPaint.setColor(mTextColor);
				mPaint.setAlpha(mTextAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				mPaint.setTextSize(mTextSize);

				mRectIcon.setEmpty();
				mRectText.setEmpty();
				Drawable drawable = getResources().getDrawable(
						mCenterCircle.getIcon());

				int h = getIconSize(drawable.getIntrinsicHeight(),
						mMinIconSize, mMaxIconSize);
				int w = getIconSize(drawable.getIntrinsicWidth(),
						mMinIconSize, mMaxIconSize);
				mRectIcon.set(mXPosition - w / 2, mYPosition - h / 2,
						mXPosition + w / 2, mYPosition + h / 2);

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
					float textLeft = mXPosition - mRectText.width() / 2;
					textBottom += mRectText.height() + 3;
					c.drawText(stringArray[j], textLeft - mRectText.left,
							textBottom - mRectText.bottom, mPaint);
				}

				drawable.setBounds(mRectIcon);
				drawable.setAlpha(mPictureAlpha);
				drawable.draw(c);
			} else if (mCenterCircle.getIcon() != 0) {
				mRect1.setEmpty();

				Drawable drawable = getResources().getDrawable(
						mCenterCircle.getIcon());

				int h = getIconSize(drawable.getIntrinsicHeight(),
						mMinIconSize, mMaxIconSize);
				int w = getIconSize(drawable.getIntrinsicWidth(),
						mMinIconSize, mMaxIconSize);
				mRect1.set(mXPosition - w / 2, mYPosition - h / 2,
						mXPosition + w / 2, mYPosition + h / 2);

				drawable.setBounds(mRect1);
				drawable.setAlpha(mPictureAlpha);
				drawable.draw(c);
			} else {
				mPaint.setColor(mTextColor);
				mPaint.setAlpha(mTextAlpha);
				mPaint.setStyle(Paint.Style.FILL);
				mPaint.setTextSize(mTextSize);

				String menuItemName = mCenterCircle.getLabel();
				String[] stringArray = menuItemName.split("\n");

				mRect1.setEmpty();
				float textHeight = 0.0F;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRect1);
					textHeight += mRect1.height() + 3;
				}

				float textBottom = mYPosition - textHeight / 2.0F;
				for (int j = 0; j < stringArray.length; j++) {
					mPaint.getTextBounds(stringArray[j], 0,
							stringArray[j].length(), mRect1);
					float textLeft = mXPosition - mRect1.width() / 2;
					textBottom += mRect1.height() + 3;
					c.drawText(stringArray[j], textLeft - mRect1.left, textBottom
							- mRect1.bottom, mPaint);
				}

			}

		}

		if (mHeaderString != null) {
			mPaint.setTextSize(mHeaderTextSize);
			mPaint.getTextBounds(mHeaderString, 0,
					mHeaderString.length(), mTextRect);
			if (!mHeaderBoxBounded) {
				determineHeaderBox();
				mHeaderBoxBounded = true;
			}

			mPaint.setColor(mOutlineColor);
			mPaint.setAlpha(mOutlineAlpha);
			mPaint.setStyle(Paint.Style.STROKE);
			c.drawRoundRect(mTextBoxRect, scalePx(5), scalePx(5), mPaint);
			mPaint.setColor(mHeaderBackgroundColor);
			mPaint.setAlpha(mHeaderBackgroundAlpha);
			mPaint.setStyle(Paint.Style.FILL);
			c.drawRoundRect(mTextBoxRect, scalePx(5), scalePx(5), mPaint);

			mPaint.setColor(mHeaderTextColor);
			mPaint.setAlpha(mHeaderTextAlpha);
			mPaint.setStyle(Paint.Style.FILL);
			mPaint.setTextSize(mHeaderTextSize);
			c.drawText(mHeaderString, mHeaderTextLeft,
					mHeaderTextBottom, mPaint);
		}
	}

	private int scalePx(int dpSize) {
		int pxSize = (int) (dpSize * mScreenDensity + 0.5F);
		return pxSize;
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

	private void animateOuterWedges(int animationDirection) {
		boolean animationComplete = false;

		float slice2 = 360 / mWedgeQty2;
		float startSlice2 = 270.0F - slice2 / 2.0F;

		double rSlice2 = 6.283185307179586D / mWedgeQty2;
		double rStart2 = 4.71238898038469D - rSlice2 / 2.0D;

		mWedges2 = new RadialMenuWedge[mWedgeQty2];
		mIconRect2 = new Rect[mWedgeQty2];

		mWedge2Shown = true;

		int wedgeSizeChange = (mR2MaxSize - mR2MinSize)
				/ mAnimateSections;

		if (animationDirection == 2) {
			if (mR2MinSize + mR2VariableSize + wedgeSizeChange < mR2MaxSize) {
				mR2VariableSize += wedgeSizeChange;
			} else {
				mAnimateOuterOut = false;
				mR2VariableSize = (mR2MaxSize - mR2MinSize);
				animationComplete = true;
			}

			mAnimateTextSize = (mTextSize / mAnimateSections * (mR2VariableSize / wedgeSizeChange));

			for (int i = 0; i < mWedges2.length; i++) {
				mWedges2[i] = new RadialMenuWedge(mXPosition,
						mYPosition, mR2MinSize, mR2MinSize
						+ mR2VariableSize, i * slice2
						+ startSlice2, slice2);
				float xCenter = (float) (Math.cos(rSlice2 * i + rSlice2 * 0.5D
						+ rStart2)
						* (mR2MinSize + mR2VariableSize + mR2MinSize) / 2.0D)
						+ mXPosition;
				float yCenter = (float) (Math.sin(rSlice2 * i + rSlice2 * 0.5D
						+ rStart2)
						* (mR2MinSize + mR2VariableSize + mR2MinSize) / 2.0D)
						+ mYPosition;

				int h = mMaxIconSize;
				int w = mMaxIconSize;
				if (((RadialMenuItem) mWedge2Data.getChildren().get(i))
						.getIcon() != 0) {
					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) mWedge2Data.getChildren()
									.get(i)).getIcon());
					h = getIconSize(drawable.getIntrinsicHeight(),
							mMinIconSize, mMaxIconSize);
					w = getIconSize(drawable.getIntrinsicWidth(),
							mMinIconSize, mMaxIconSize);
				}

				if (mR2VariableSize < h) {
					h = mR2VariableSize;
				}
				if (mR2VariableSize < w) {
					w = mR2VariableSize;
				}

				mIconRect2[i] = new Rect((int) xCenter - w / 2,
						(int) yCenter - h / 2, (int) xCenter + w / 2,
						(int) yCenter + h / 2);

				int widthOffset = mMaxSize;
				if (widthOffset < mTextRect.width() / 2) {
					widthOffset = mTextRect.width() / 2 + scalePx(3);
				}
				mTextBoxRect.set(mXPosition - widthOffset,
						mYPosition - (mR2MinSize + mR2VariableSize)
						- mHeaderBuffer - mTextRect.height()
						- scalePx(3), mXPosition + widthOffset,
						mYPosition - (mR2MinSize + mR2VariableSize)
						- mHeaderBuffer + scalePx(3));
				mHeaderTextBottom = (mYPosition
						- (mR2MinSize + mR2VariableSize)
						- mHeaderBuffer - mTextRect.bottom);
			}

		} else if (animationDirection == 1) {
			if (mR2MinSize < mR2MaxSize - mR2VariableSize
					- wedgeSizeChange) {
				mR2VariableSize += wedgeSizeChange;
			} else {
				mAnimateOuterIn = false;
				mR2VariableSize = mR2MaxSize;
				animationComplete = true;
			}

			mAnimateTextSize = (mTextSize - mTextSize
					/ mAnimateSections
					* (mR2VariableSize / wedgeSizeChange));

			for (int i = 0; i < mWedges2.length; i++) {
				mWedges2[i] = new RadialMenuWedge(mXPosition,
						mYPosition, mR2MinSize, mR2MaxSize
						- mR2VariableSize, i * slice2
						+ startSlice2, slice2);

				float xCenter = (float) (Math.cos(rSlice2 * i + rSlice2 * 0.5D
						+ rStart2)
						* (mR2MaxSize - mR2VariableSize + mR2MinSize) / 2.0D)
						+ mXPosition;
				float yCenter = (float) (Math.sin(rSlice2 * i + rSlice2 * 0.5D
						+ rStart2)
						* (mR2MaxSize - mR2VariableSize + mR2MinSize) / 2.0D)
						+ mYPosition;

				int h = mMaxIconSize;
				int w = mMaxIconSize;
				if (((RadialMenuItem) mWedge2Data.getChildren().get(i))
						.getIcon() != 0) {
					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) mWedge2Data.getChildren()
									.get(i)).getIcon());
					h = getIconSize(drawable.getIntrinsicHeight(),
							mMinIconSize, mMaxIconSize);
					w = getIconSize(drawable.getIntrinsicWidth(),
							mMinIconSize, mMaxIconSize);
				}

				if (mR2MaxSize - mR2MinSize - mR2VariableSize < h) {
					h = mR2MaxSize - mR2MinSize - mR2VariableSize;
				}
				if (mR2MaxSize - mR2MinSize - mR2VariableSize < w) {
					w = mR2MaxSize - mR2MinSize - mR2VariableSize;
				}

				mIconRect2[i] = new Rect((int) xCenter - w / 2,
						(int) yCenter - h / 2, (int) xCenter + w / 2,
						(int) yCenter + h / 2);

				int heightOffset = mR2MaxSize - mR2VariableSize;
				int widthOffset = mMaxSize;
				if (mMaxSize > mR2MaxSize - mR2VariableSize) {
					heightOffset = mMaxSize;
				}
				if (widthOffset < mTextRect.width() / 2) {
					widthOffset = mTextRect.width() / 2 + scalePx(3);
				}
				mTextBoxRect
				.set(mXPosition - widthOffset, mYPosition
						- heightOffset - mHeaderBuffer
						- mTextRect.height() - scalePx(3),
						mXPosition + widthOffset, mYPosition
						- heightOffset - mHeaderBuffer
						+ scalePx(3));
				mHeaderTextBottom = (mYPosition - heightOffset
						- mHeaderBuffer - mTextRect.bottom);
			}

		}

		if (animationComplete) {
			mR2VariableSize = 0;
			mAnimateTextSize = mTextSize;
			if (animationDirection == 1) {
				mWedge2Shown = false;
			}
		}

		invalidate();
	}

	private void determineWedges() {
		int entriesQty = mMenuEntries.size();
		if (entriesQty > 0) {
			mWedgeQty = entriesQty;

			float degSlice = 360 / mWedgeQty;
			float startDegSlice = 270.0F - degSlice / 2.0F;

			double rSlice = 6.283185307179586D / mWedgeQty;
			double rStart = 4.71238898038469D - rSlice / 2.0D;

			mWedges = new RadialMenuWedge[mWedgeQty];
			mIconRect = new Rect[mWedgeQty];

			for (int i = 0; i < mWedges.length; i++) {
				mWedges[i] = new RadialMenuWedge(mXPosition,
						mYPosition, mMinSize, mMaxSize, i
						* degSlice + startDegSlice, degSlice);
				float xCenter = (float) (Math.cos(rSlice * i + rSlice * 0.5D
						+ rStart)
						* (mMaxSize + mMinSize) / 2.0D)
						+ mXPosition;
				float yCenter = (float) (Math.sin(rSlice * i + rSlice * 0.5D
						+ rStart)
						* (mMaxSize + mMinSize) / 2.0D)
						+ mYPosition;

				int h = mMaxIconSize;
				int w = mMaxIconSize;
				if (((RadialMenuItem) mMenuEntries.get(i)).getIcon() != 0) {
					Drawable drawable = getResources().getDrawable(
							((RadialMenuItem) mMenuEntries.get(i))
							.getIcon());
					h = getIconSize(drawable.getIntrinsicHeight(),
							mMinIconSize, mMaxIconSize);
					w = getIconSize(drawable.getIntrinsicWidth(),
							mMinIconSize, mMaxIconSize);
				}

				mIconRect[i] = new Rect((int) xCenter - w / 2,
						(int) yCenter - h / 2, (int) xCenter + w / 2,
						(int) yCenter + h / 2);
			}

			invalidate();
		}
	}

	private void determineOuterWedges(RadialMenuItem entry) {
		int entriesQty = entry.getChildren().size();
		mWedgeQty2 = entriesQty;

		float degSlice2 = 360 / mWedgeQty2;
		float startDegSlice2 = 270.0F - degSlice2 / 2.0F;

		double rSlice2 = 6.283185307179586D / mWedgeQty2;
		double rStart2 = 4.71238898038469D - rSlice2 / 2.0D;

		mWedges2 = new RadialMenuWedge[mWedgeQty2];
		mIconRect2 = new Rect[mWedgeQty2];

		for (int i = 0; i < mWedges2.length; i++) {
			mWedges2[i] = new RadialMenuWedge(mXPosition,
					mYPosition, mR2MinSize, mR2MaxSize, i
					* degSlice2 + startDegSlice2, degSlice2);
			float xCenter = (float) (Math.cos(rSlice2 * i + rSlice2 * 0.5D
					+ rStart2)
					* (mR2MaxSize + mR2MinSize) / 2.0D)
					+ mXPosition;
			float yCenter = (float) (Math.sin(rSlice2 * i + rSlice2 * 0.5D
					+ rStart2)
					* (mR2MaxSize + mR2MinSize) / 2.0D)
					+ mYPosition;

			int h = mMaxIconSize;
			int w = mMaxIconSize;
			if (((RadialMenuItem) entry.getChildren().get(i)).getIcon() != 0) {
				Drawable drawable = getResources()
						.getDrawable(
								((RadialMenuItem) entry.getChildren().get(i))
								.getIcon());
				h = getIconSize(drawable.getIntrinsicHeight(),
						mMinIconSize, mMaxIconSize);
				w = getIconSize(drawable.getIntrinsicWidth(), mMinIconSize,
						mMaxIconSize);
			}
			mIconRect2[i] = new Rect((int) xCenter - w / 2, (int) yCenter
					- h / 2, (int) xCenter + w / 2, (int) yCenter + h / 2);
		}
		mWedge2Data = entry;
		invalidate();
	}

	private void determineHeaderBox() {
		mHeaderTextLeft = (mXPosition - mTextRect.width() / 2);
		mHeaderTextBottom = (mYPosition - mMaxSize
				- mHeaderBuffer - mTextRect.bottom);
		int offset = mMaxSize;
		if (offset < mTextRect.width() / 2) {
			offset = mTextRect.width() / 2 + scalePx(3);
		}
		mTextBoxRect.set(mXPosition - offset, mYPosition
				- mMaxSize - mHeaderBuffer - mTextRect.height()
				- scalePx(3), mXPosition + offset, mYPosition
				- mMaxSize - mHeaderBuffer + scalePx(3));
	}

	public void addMenuEntry(List<RadialMenuItem> menuItems) {
		mMenuEntries.addAll(menuItems);
		determineWedges();
	}

	public void addMenuEntry(RadialMenuItem menuItem) {
		mMenuEntries.add(menuItem);
		determineWedges();
	}

	public void setCenterCircle(RadialMenuItem menuItem) {
		mCenterCircle = menuItem;
	}

	public void setInnerRingRadius(int innerRadius, int outerRadius) {
		mMinSize = scalePx(innerRadius);
		mMaxSize = scalePx(outerRadius);
		determineWedges();
	}

	public void setOuterRingRadius(int innerRadius, int outerRadius) {
		mR2MinSize = scalePx(innerRadius);
		mR2MaxSize = scalePx(outerRadius);
		determineWedges();
	}

	public void setCenterCircleRadius(int centerRadius) {
		mCRadius = scalePx(centerRadius);
		determineWedges();
	}

	public void setTextSize(int textSize) {
		mTextSize = scalePx(textSize);
		mAnimateTextSize = mTextSize;
	}

	public void setIconSize(int minIconSize, int maxIconSize) {
		mMinIconSize = scalePx(minIconSize);
		mMaxIconSize = scalePx(maxIconSize);
		determineWedges();
	}

	public void setCenterLocation(int x, int y) {
		mXPosition = x;
		mYPosition = y;
		determineWedges();
		mHelper.onOpenAnimation(this, mXPosition, mYPosition,
				mXSource, mYSource);
	}

	public void setSourceLocation(int x, int y) {
		mXSource = x;
		mYSource = y;
		mHelper.onOpenAnimation(this, mXPosition, mYPosition,
				mXSource, mYSource);
	}

	/** @deprecated */
	public void setShowSourceLocation(boolean showSourceLocation) {
		mShowSource = showSourceLocation;
		mHelper.onOpenAnimation(this, mXPosition, mYPosition,
				mXSource, mYSource);
	}

	public void setAnimationSpeed(long millis) {
		mHelper.onOpenAnimation(this, mXPosition, mYPosition,
				mXSource, mYSource, millis);
	}

	public void setInnerRingColor(int color, int alpha) {
		mDefaultColor = color;
		mDefaultAlpha = alpha;
	}

	public void setOuterRingColor(int color, int alpha) {
		mWedge2Color = color;
		mWedge2Alpha = alpha;
	}

	public void setOutlineColor(int color, int alpha) {
		mOutlineColor = color;
		mOutlineAlpha = alpha;
	}

	public void setSelectedColor(int color, int alpha) {
		mSelectedColor = color;
		mSelectedAlpha = alpha;
	}

	public void setDisabledColor(int color, int alpha) {
		mDisabledColor = color;
		mDisabledAlpha = alpha;
	}

	public void setTextColor(int color, int alpha) {
		mTextColor = color;
		mTextAlpha = alpha;
	}

	public void setHeader(String header, int TextSize) {
		mHeaderString = header;
		mHeaderTextSize = scalePx(TextSize);
		mHeaderBoxBounded = false;
	}

	public void setHeaderColors(int textColor, int textAlpha, int bgColor, int bgAlpha) {
		mHeaderTextColor = textColor;
		mHeaderTextAlpha = textAlpha;
		mHeaderBackgroundColor = bgColor;
		mHeaderBackgroundAlpha = bgAlpha;
	}

	public void show(View anchor, int posX, int posY) {
		mWindow.setContentView(this);
		mWindow.showAtLocation(anchor, 0, posX, posY);
	}

	public void show(View anchor) {
		mWindow.setContentView(this);
		mWindow.showAtLocation(anchor, 0, mXSource, mYSource);
	}

	public void dismiss() {
		if (mWindow != null)
			mWindow.dismiss();
	}
}