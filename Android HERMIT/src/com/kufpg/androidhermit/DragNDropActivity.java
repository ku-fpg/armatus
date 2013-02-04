package com.kufpg.androidhermit;

import android.content.ClipData;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.DragEvent;
import android.view.HapticFeedbackConstants;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.DragShadowBuilder;
import android.view.View.OnDragListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.widget.LinearLayout;

public class DragNDropActivity extends StandardActivity {
	/** Called when the activity is first created. */

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.drag_n_drop);
		findViewById(R.id.myimage1).setOnTouchListener(new MyTouchListener());
		findViewById(R.id.myimage2).setOnTouchListener(new MyTouchListener());
		findViewById(R.id.myimage3).setOnTouchListener(new MyTouchListener());
		findViewById(R.id.myimage4).setOnTouchListener(new MyTouchListener());
		findViewById(R.id.topleft).setOnDragListener(new MyDragListener());
		findViewById(R.id.topright).setOnDragListener(new MyDragListener());
		findViewById(R.id.bottomleft).setOnDragListener(new MyDragListener());
		findViewById(R.id.bottomright).setOnDragListener(new MyDragListener());

	}

	private final class MyTouchListener implements OnTouchListener {
		public boolean onTouch(View view, MotionEvent motionEvent) {
			if (motionEvent.getAction() == MotionEvent.ACTION_DOWN) {
				ClipData data = ClipData.newPlainText("", "");
				DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
				view.startDrag(data, shadowBuilder, view, 0);
				view.setVisibility(View.INVISIBLE);
				return true;
			} else {
				return false;
			}
		}
	}

	class MyDragListener implements OnDragListener {
		Drawable enterShape = getResources().getDrawable(android.R.color.white);
		Drawable normalShape = getResources().getDrawable(android.R.color.black);

		@Override
		public boolean onDrag(View v, DragEvent event) {
			int action = event.getAction();
			switch (event.getAction()) {
			case DragEvent.ACTION_DRAG_STARTED:
				v.performHapticFeedback(HapticFeedbackConstants.FLAG_IGNORE_VIEW_SETTING);
				break;
			case DragEvent.ACTION_DRAG_ENTERED:
				v.setBackground(enterShape);
				break;
			case DragEvent.ACTION_DRAG_EXITED:
				v.setBackground(normalShape);
				break;
			case DragEvent.ACTION_DROP:
				// Dropped, reassign View to ViewGroup
				View view = (View) event.getLocalState();
				ViewGroup owner = (ViewGroup) view.getParent();
				owner.removeView(view);
				LinearLayout container = (LinearLayout) v;
				container.addView(view);
				view.setVisibility(View.VISIBLE);
				break;
			case DragEvent.ACTION_DRAG_ENDED:
				v.setBackground(normalShape);
			default:
				break;
			}
			return true;
		}
	}
} 
