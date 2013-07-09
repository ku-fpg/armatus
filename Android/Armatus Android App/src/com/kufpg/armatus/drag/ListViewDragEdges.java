package com.kufpg.armatus.drag;

import android.content.Context;
import android.os.SystemClock;
import android.util.AttributeSet;
import android.view.DragEvent;
import android.view.MotionEvent;
import android.view.View;
import android.view.accessibility.AccessibilityEvent;
import android.view.accessibility.AccessibilityNodeInfo;
import android.widget.ListView;
import android.widget.RelativeLayout;

public class ListViewDragEdges extends RelativeLayout {
	private static final int TIME_PER_ENTRY = 200;
	private static final int EDGE_HEIGHT = 20;
	private ListView mListView;
	private DragEdge mTopEdge, mBottomEdge;
	private LayoutParams mTopParams, mBottomParams;

	public ListViewDragEdges(Context context) {
		super(context);
		init(context);
	}
	
	public ListViewDragEdges(Context context, AttributeSet attrs) {
		super(context, attrs);
		init(context);
	}

	
	public ListViewDragEdges(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init(context);
	}
	
	@Override
	protected void onFinishInflate() {
		super.onFinishInflate();
		
		mListView = (ListView) getChildAt(0);
		mTopParams.addRule(RelativeLayout.ALIGN_TOP, mListView.getId());
		mBottomParams.addRule(RelativeLayout.ALIGN_BOTTOM, mListView.getId());
		addView(mTopEdge, mTopParams);
		addView(mBottomEdge, mBottomParams);
	}
	
	private void init(Context context) {
		mTopEdge = new DragEdge(context) {
			@Override
			public boolean onDrag(View v, DragEvent event) {
				switch (event.getAction()) {
				case DragEvent.ACTION_DRAG_ENTERED:
					int viewsAbove = mListView.getFirstVisiblePosition();
					mListView.smoothScrollToPositionFromTop(0, 0, viewsAbove * TIME_PER_ENTRY);
					return true;
				default:
					return super.onDrag(v, event);
				}
			}
		};
		mBottomEdge = new DragEdge(context) {
			@Override
			public boolean onDrag(View v, DragEvent event) {
				switch (event.getAction()) {
				case DragEvent.ACTION_DRAG_ENTERED:
					int totalViews = mListView.getAdapter().getCount();
					int viewsBelow = totalViews - mListView.getLastVisiblePosition();
					mListView.smoothScrollToPositionFromTop(totalViews, 0, viewsBelow * TIME_PER_ENTRY);
					return true;
				default:
					return super.onDrag(v, event);
				}
			}
		};
		mTopParams = new LayoutParams(LayoutParams.MATCH_PARENT, EDGE_HEIGHT);
		mBottomParams = new LayoutParams(LayoutParams.MATCH_PARENT, EDGE_HEIGHT);
	}
	
	@Override
    public void onInitializeAccessibilityEvent(AccessibilityEvent event) {
        super.onInitializeAccessibilityEvent(event);
        event.setClassName(ListViewDragEdges.class.getName());
    }

    @Override
    public void onInitializeAccessibilityNodeInfo(AccessibilityNodeInfo info) {
        super.onInitializeAccessibilityNodeInfo(info);
        info.setClassName(ListViewDragEdges.class.getName());
    }
    
    private void stopListScroll() {
    	MotionEvent cancel = MotionEvent.obtain(SystemClock.uptimeMillis(),
				SystemClock.uptimeMillis(),	MotionEvent.ACTION_CANCEL, 0, 0, 0);
		mListView.dispatchTouchEvent(cancel);
		cancel.recycle();
    }
    
    private class DragEdge extends View implements OnDragListener {
    	
    	public DragEdge(Context context) {
    		super(context);
    		init();
    	}
    	
    	public DragEdge(Context context, AttributeSet attrs) {
    		super(context, attrs);
    		init();
    	}

    	public DragEdge(Context context, AttributeSet attrs, int defStyle) {
    		super(context, attrs, defStyle);
    		init();
    	}
    	
    	private void init() {
    		setBackgroundColor(getResources().getColor(android.R.color.transparent));
    		setOnDragListener(this);
    	}

		@Override
		public boolean onDrag(View v, DragEvent event) {
			switch (event.getAction()) {
			case DragEvent.ACTION_DROP:
			case DragEvent.ACTION_DRAG_ENDED:
			case DragEvent.ACTION_DRAG_EXITED:
				stopListScroll();
				break;
			}
			return true;
		}
    	
    }
}
