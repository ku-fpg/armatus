package com.kufpg.androidhermit.drag;

import com.slidingmenu.lib.SlidingMenu;

import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.LinearLayout;

/**
 * A special layout that stores CommandIcons and closes a SlidingMenu when one of
 * its CommandIcons begins a DragEvent.
 */
public class DragLayout extends LinearLayout {
	private SlidingMenu mSlidingMenu;

	public DragLayout(Context context) {
		this(context, null, 0);
	}

	public DragLayout(Context context, AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public DragLayout(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);

		setOnDragListener(new DragSinkListener() {
			@Override
			public void onDragStarted(View dragView, View dragSink) {
				mSlidingMenu.showContent();
			}
		});
	}

	public void setSlidingMenu(SlidingMenu menu) {
		mSlidingMenu = menu;
	}

}
