package edu.kufpg.armatus.activity;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import android.util.AttributeSet;
import android.widget.ScrollView;

import com.google.common.collect.Lists;

public class ScopeScrollView extends ScrollView {

	private final Paint PAINT = new Paint();
	private int mScreenOffset;
	private int mScrollOffset;
	private Rect mMainRect, mParentRect;
	private List<Rect> mChildRects = new ArrayList<Rect>();

	public ScopeScrollView(Context context) {
		super(context);
		init();
	}

	public ScopeScrollView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public ScopeScrollView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}

	private void init() {
		PAINT.setAntiAlias(true);
		PAINT.setColor(Color.YELLOW);
		PAINT.setStrokeJoin(Paint.Join.ROUND);
		PAINT.setStrokeWidth(6f);
		PAINT.setStyle(Paint.Style.STROKE);
	}
	
	public void clearLines() {
		mMainRect = null;
		mParentRect = null;
		mChildRects.clear();
	}

	public void drawParentalLine(Rect mainRect, Rect parentRect) {
		mMainRect = mainRect;
		mParentRect = parentRect;
		mChildRects.clear();
		mScrollOffset = getScrollY();
		int[] screenCoords = new int[2];
		getLocationOnScreen(screenCoords);
		mScreenOffset = screenCoords[1];
	}

	public void drawChildLines(Rect mainRect, Rect... childRects) {
		mMainRect = mainRect;
		mParentRect = null;
		mChildRects = Lists.newArrayList(childRects);
		mScrollOffset = getScrollY();
		int[] screenCoords = new int[2];
		getLocationOnScreen(screenCoords);
		mScreenOffset = screenCoords[1];
	}

	@Override
	protected void onDraw(Canvas canvas) {
		if (mMainRect != null) {
			if (mParentRect != null) {
				canvas.drawLine(mParentRect.right, mParentRect.bottom + mScrollOffset - mScreenOffset,
						mMainRect.right, mMainRect.bottom + mScrollOffset - mScreenOffset, PAINT);
			}
			if (mChildRects != null) {
				for (Rect rect : mChildRects) {
					canvas.drawLine(mMainRect.right, mMainRect.bottom + mScrollOffset - mScreenOffset,
							rect.right, rect.bottom + mScrollOffset - mScreenOffset, PAINT);
				}
			}
		}

		super.onDraw(canvas);
	}
}