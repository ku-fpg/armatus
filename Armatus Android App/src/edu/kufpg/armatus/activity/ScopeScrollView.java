package edu.kufpg.armatus.activity;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.AttributeSet;
import android.widget.ScrollView;

import com.google.common.collect.Lists;

public class ScopeScrollView extends ScrollView {

    private final Paint PAINT = new Paint() {{
        setAntiAlias(true);
        setColor(Color.YELLOW);
        setStrokeJoin(Paint.Join.ROUND);
        setStrokeWidth(6.0f);
        setStyle(Paint.Style.STROKE);
    }};
    private int mScreenOffset;
    private int mScrollOffset;
    @Nullable private Rect mMainRect, mParentRect;
    private List<Rect> mChildRects = new ArrayList<Rect>();

    public ScopeScrollView(@NonNull final Context context) {
        super(context);
    }

    public ScopeScrollView(@NonNull final Context context, @NonNull final AttributeSet attrs) {
        super(context, attrs);
    }

    public ScopeScrollView(@NonNull final Context context,
                           @NonNull final AttributeSet attrs,
                           final int defStyle) {
        super(context, attrs, defStyle);
    }

    public void clearLines() {
        mMainRect = null;
        mParentRect = null;
        mChildRects.clear();
    }

    public void drawParentalLine(@Nullable final Rect mainRect, @Nullable final Rect parentRect) {
        mMainRect = mainRect;
        mParentRect = parentRect;
        mChildRects.clear();
        mScrollOffset = getScrollY();
        final int[] screenCoords = new int[2];
        getLocationOnScreen(screenCoords);
        mScreenOffset = screenCoords[1];
    }

    public void drawChildLines(@Nullable final Rect mainRect, @NonNull final Rect... childRects) {
        mMainRect = mainRect;
        mParentRect = null;
        mChildRects = Lists.newArrayList(childRects);
        mScrollOffset = getScrollY();
        final int[] screenCoords = new int[2];
        getLocationOnScreen(screenCoords);
        mScreenOffset = screenCoords[1];
    }

    @Override protected void onDraw(@NonNull final Canvas canvas) {
        if (mMainRect != null) {
            if (mParentRect != null) {
                canvas.drawLine(mParentRect.right, mParentRect.bottom + mScrollOffset - mScreenOffset,
                        mMainRect.right, mMainRect.bottom + mScrollOffset - mScreenOffset, PAINT);
            }
            if (mChildRects != null) {
                for (final Rect rect : mChildRects) {
                    canvas.drawLine(mMainRect.right, mMainRect.bottom + mScrollOffset - mScreenOffset,
                            rect.right, rect.bottom + mScrollOffset - mScreenOffset, PAINT);
                }
            }
        }

        super.onDraw(canvas);
    }
}