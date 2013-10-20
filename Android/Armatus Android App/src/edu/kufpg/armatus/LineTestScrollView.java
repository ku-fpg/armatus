package edu.kufpg.armatus;

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

public class LineTestScrollView extends ScrollView {

	private final Paint PAINT = new Paint();
	private Rect mMainRect, mParentRect;
	private List<Rect> mChildRects = new ArrayList<Rect>();

	public LineTestScrollView(Context context) {
		super(context);
		init();
	}

	public LineTestScrollView(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public LineTestScrollView(Context context, AttributeSet attrs, int defStyle) {
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
	}

	public void drawChildLines(Rect mainRect, Rect... childRects) {
		mMainRect = mainRect;
		mParentRect = null;
		mChildRects = Lists.newArrayList(childRects);
	}

	@Override
	protected void onDraw(Canvas canvas) {
		if (mMainRect != null) {
			if (mParentRect != null) {
				canvas.drawLine(mParentRect.right, mParentRect.bottom,
						mMainRect.right, mMainRect.bottom, PAINT);
			}
			if (mChildRects != null) {
				for (Rect rect : mChildRects) {
					canvas.drawLine(mMainRect.right, mMainRect.bottom,
							rect.right, rect.bottom, PAINT);
				}
			}
		}

		super.onDraw(canvas);
	}

//	@Override
//	public Parcelable onSaveInstanceState() {
//		Parcelable superState = super.onSaveInstanceState();
//		SavedState ss = new SavedState(superState);
//		ss.mainRect = mMainRect;
//		ss.parentRect = mParentRect;
//		ss.childRects = mChildRects;
//		return ss;
//	}
//
//	@Override
//	public void onRestoreInstanceState(Parcelable state) {
//		if (!(state instanceof SavedState)) {
//			super.onRestoreInstanceState(state);
//		}
//
//		SavedState ss = (SavedState) state;
//		super.onRestoreInstanceState(ss.getSuperState());
//		mMainRect = ss.mainRect;
//		mParentRect = ss.parentRect;
//		mChildRects = ss.childRects;
//	}
//
//	protected static class SavedState extends BaseSavedState {
//		Rect mainRect, parentRect;
//		ArrayList<Rect> childRects;
//
//		SavedState(Parcelable superState) {
//			super(superState);
//		}
//
//		@Override
//		public void writeToParcel(Parcel dest, int flags) {
//			super.writeToParcel(dest, flags);
//			dest.writeParcelable(mainRect, flags);
//			dest.writeParcelable(parentRect, flags);
//			dest.writeTypedList(childRects);
//		}
//
//		public static final Parcelable.Creator<SavedState> CREATOR
//		= new Parcelable.Creator<SavedState>() {
//			public SavedState createFromParcel(Parcel in) {
//				return new SavedState(in);
//			}
//
//			public SavedState[] newArray(int size) {
//				return new SavedState[size];
//			}
//		};
//
//		private SavedState(Parcel in) {
//			super(in);
//			mainRect = in.readParcelable(Rect.class.getClassLoader());
//			parentRect = in.readParcelable(Rect.class.getClassLoader());
//			in.readTypedList(childRects, Rect.CREATOR);
//		}
//	}
}