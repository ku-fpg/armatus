package com.kufpg.androidhermit.drag;

import com.kufpg.androidhermit.R;

import android.content.ClipData;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.DragShadowBuilder;
import android.view.View.OnLongClickListener;
import android.view.View.OnTouchListener;

public class DragSourceClickListener implements OnTouchListener, OnLongClickListener {	
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
			v.setBackground(v.getResources().getDrawable(R.drawable.console_text_border));
		} else if(event.getAction() == MotionEvent.ACTION_UP
				|| event.getAction() == MotionEvent.ACTION_OUTSIDE) {
			v.setBackgroundColor(v.getResources().getColor(android.R.color.transparent));
		}
		return false;	
	}

}
