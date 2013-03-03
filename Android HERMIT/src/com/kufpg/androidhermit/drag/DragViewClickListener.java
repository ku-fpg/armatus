package com.kufpg.androidhermit.drag;

import com.kufpg.androidhermit.R;

import android.content.ClipData;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.DragShadowBuilder;
import android.view.View.OnLongClickListener;
import android.view.View.OnTouchListener;

/**
 * Allows a View to be dragged upon a long click.
 */
public class DragViewClickListener implements OnTouchListener, OnLongClickListener {	
	@Override
	public boolean onLongClick(View v) {
		ClipData dragData = ClipData.newPlainText("", "");
		DragShadowBuilder shadow = new DragShadowBuilder(v);
		v.startDrag(dragData, shadow, v, 0);
		v.setVisibility(View.INVISIBLE);
		return false;
	}

	@Override
	public boolean onTouch(View v, MotionEvent event) {
		if(event.getAction() == MotionEvent.ACTION_DOWN) {
			v.setBackground(v.getResources().getDrawable(R.drawable.console_entry_highlighted));
		} else if(event.getAction() == MotionEvent.ACTION_UP
				|| event.getAction() == MotionEvent.ACTION_OUTSIDE) {
			v.setBackgroundColor(v.getResources().getColor(android.R.color.transparent));
		}
		return false;	
	}

}
