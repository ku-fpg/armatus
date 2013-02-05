package com.kufpg.androidhermit.util.drag;

import com.slidingmenu.lib.SlidingMenu;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.LinearLayout;

public class DragImageViewLayout extends LinearLayout {
	private SlidingMenu mSlidingMenu;

	public DragImageViewLayout(Context context) {
		super(context);
		setupView();
	}

	public DragImageViewLayout(Context context, AttributeSet attrs) {
		super(context, attrs);
		setupView();
	}

	public DragImageViewLayout(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		setupView();
	}

	public DragImageViewLayout(Context context, SlidingMenu draggableViewSource) {
		super(context);
		setupView();
	}
	
	public void setSlidingMenu(SlidingMenu menu) {
		mSlidingMenu = menu;
	}

	private void setupView() {
		setOnDragListener(new DragSinkListener() {
			@Override
			public void onDragStarted(View dragSource, View dragSink) {
				mSlidingMenu.showContent();
			}
		});
	}

}
