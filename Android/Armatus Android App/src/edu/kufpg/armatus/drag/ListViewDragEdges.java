package edu.kufpg.armatus.drag;

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

/**
 * Contains functions for dragging icons on edges. Also has values for the edge height and time per entry. 
 * Extends {@link android.widget.RelativeLayout RelativeLayout} class.
 */
public class ListViewDragEdges extends RelativeLayout {
	private static final int TIME_PER_ENTRY = 200;
	private static final int EDGE_HEIGHT = 20;
	private ListView mListView;
	private DragEdge mTopEdge, mBottomEdge;
	private LayoutParams mTopParams, mBottomParams;

	/**
	 * List View Drag Edges Function
	 * Able to drag from edges of images with getting the {@link Android.Content.Context context}
	 * @param {@link Android.Content.Context context}
	 */
	public ListViewDragEdges(Context context) {
		super(context);
		init(context);
	}
	
	/**
	 * List View Drag Edges with attributes function
	 * Able to drag from edges of images with getting the context and the attributes
	 * {@link Android.Conent.Context} {@link Android.util.AttributeSet}
	 * @param {@link Android.Content.Context context}
	 * @param {@link android.util.AttributeSet attrs}
	 */
	public ListViewDragEdges(Context context, AttributeSet attrs) {
		super(context, attrs);
		init(context);
	}

	/**
	 * List View Drag Edges with attributes function
	 * Able to drag from edges of images with getting the context, attributes and style
	 * {@link Android.Conent.Context} {@link Android.util.AttributeSet}
	 * @param {@link Android.Content.Context context}
	 * @param {@link android.util.AttributeSet attrs}
	 * @param defStyle
	 */
	public ListViewDragEdges(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init(context);
	}
	
	/**
	 * Function to override Inflate using {@link android.widget.ListView ListView} and {@link android.widget.RelativeLayout RelativeLayout}
	 */
	@Override
	protected void onFinishInflate() {
		super.onFinishInflate();
		
		mListView = (ListView) getChildAt(0);
		mTopParams.addRule(RelativeLayout.ALIGN_TOP, mListView.getId());
		mBottomParams.addRule(RelativeLayout.ALIGN_BOTTOM, mListView.getId());
		addView(mTopEdge, mTopParams);
		addView(mBottomEdge, mBottomParams);
	}
	
	/**
	 * init Function: 
	 * Creates a {@link edu.kufpg.armatus.drag.ListViewDragEdges.DragEdge DragEdges} from the {@link Android.Content.Context Context} for top and bottom and checks for a case of a onDrag event.
	 * @param {@link Android.Content.Context context}
	 */
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
    
    /**
     * Stop List Scroll Function that stops the scrolling function
     */
    private void stopListScroll() {
    	MotionEvent cancel = MotionEvent.obtain(SystemClock.uptimeMillis(),
				SystemClock.uptimeMillis(),	MotionEvent.ACTION_CANCEL, 0, 0, 0);
		mListView.dispatchTouchEvent(cancel);
		cancel.recycle();
    }
    
    /**
     * Drag Edge Class that extends {@link android.view.View View} and implements {@link android.view.View.OnDragListener OnDragListener}. 
     */
    private class DragEdge extends View implements OnDragListener {
    	
    	/**
    	 * DragEdge function. On a drag of edge this function gets the {@link android.content.Context context} of the image. 
    	 * After that it runs it through the {@link edu.kufpg.armatus.drag.ListViewDragEdges.DragEdge.init init} function.
    	 * @param c{@link Android.Content.Context context}
    	 */
    	public DragEdge(Context context) {
    		super(context);
    		init();
    	}
    	
    	/**
    	 * DragEdge function. On a drag of edge this function gets the {@link android.content.Context context} 
    	 * and {@link android.util.AttributeSet AttributeSet} of the image. 
    	 * After that it runs it through the {@link edu.kufpg.armatus.drag.ListViewDragEdges.DragEdge.init init} function.
    	 * @param {@link Android.Content.Context context}
    	 * @param {@link andorid.util.AttributeSet attrs}
    	 */
    	public DragEdge(Context context, AttributeSet attrs) {
    		super(context, attrs);
    		init();
    	}

    	/**
    	 * DragEdge function. On a drag of edge this function gets the {@link android.content.Context context}, {@link android.util.AttributeSet AttributeSet} 
    	 * and style of the image. 
    	 * After that it runs it through the {@link edu.kufpg.armatus.drag.ListViewDragEdges.DragEdge.init init} function.
    	 * @param {@link Android.Content.Context context}
    	 * @param {@link android.util.AttributeSet attrs}
    	 * @param defStyle
    	 */
    	public DragEdge(Context context, AttributeSet attrs, int defStyle) {
    		super(context, attrs, defStyle);
    		init();
    	}
    	
    	/**
    	 * Private Void function. Sets the background to transparent then sets the {@link android.view.View.OnDragListener OnDragListener} to the "this" variable.
    	 */
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
