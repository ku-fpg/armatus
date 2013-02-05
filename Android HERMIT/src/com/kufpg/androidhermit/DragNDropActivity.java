package com.kufpg.androidhermit;

import com.kufpg.androidhermit.util.drag.DragSinkListener;
import com.kufpg.androidhermit.util.drag.DragSourceClickListener;
import com.slidingmenu.lib.SlidingMenu;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

public class DragNDropActivity extends StandardActivity {
	private SlidingMenu mSlidingMenu;
	private DragSinkListener mDragObserver = new DragSinkListener() {
		@Override
		public void onDragStarted(View dragSource, View dragSink) {
			mSlidingMenu.showContent();
		}
		@Override
		public void onDragEntered(View dragSource, View dragSink) {}
	};
	private DragSinkListener mDragObserver2 = new DragSinkListener() {
		@Override
		public void onDragEntered(View dragSource, View dragSink) {}
	};

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.drag_n_drop);
		
		LayoutInflater inflater = getLayoutInflater();
		View dragView = inflater.inflate(R.layout.drag_n_drop,
				(ViewGroup) findViewById(R.id.drag_grid));
//		dragView.findViewById(R.id.myimage1).setOnLongClickListener(mDragListener);
//		dragView.findViewById(R.id.myimage2).setOnLongClickListener(mDragListener);
//		dragView.findViewById(R.id.myimage3).setOnLongClickListener(mDragListener);
//		dragView.findViewById(R.id.myimage4).setOnLongClickListener(mDragListener);
		dragView.findViewById(R.id.topleft).setOnDragListener(mDragObserver);
		dragView.findViewById(R.id.topright).setOnDragListener(mDragObserver);
		dragView.findViewById(R.id.bottomleft).setOnDragListener(mDragObserver);
		dragView.findViewById(R.id.bottomright).setOnDragListener(mDragObserver);
		
		mSlidingMenu = new SlidingMenu(this);
		mSlidingMenu.setMode(SlidingMenu.LEFT_RIGHT);
		mSlidingMenu.setTouchModeAbove(SlidingMenu.TOUCHMODE_FULLSCREEN);
		mSlidingMenu.setFadeDegree(0.35f);
		mSlidingMenu.setShadowWidthRes(R.dimen.shadow_width);
		mSlidingMenu.setBehindOffsetRes(R.dimen.slidingmenu_offset);
		mSlidingMenu.attachToActivity(this, SlidingMenu.SLIDING_CONTENT);
		mSlidingMenu.setMenu(R.layout.drag_n_drop);
		mSlidingMenu.setSecondaryMenu(R.layout.drag_n_drop);
	}
} 
