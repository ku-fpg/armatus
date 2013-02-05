package com.kufpg.androidhermit.util.drag;

import android.view.DragEvent;
import android.view.View;
import android.view.View.OnDragListener;

public class DragSinkListener implements OnDragListener {
	public void onDragStarted(View dragSource, View dragSink) {}
	public void onDragEntered(View dragSource, View dragSink) {}
	public void onDragExited(View dragSource, View dragSink) {}
	public void onDragDropped(View dragSource, View dragSink) {}

	@Override
	public boolean onDrag(View v, DragEvent event) {
		switch (event.getAction()) {
		case DragEvent.ACTION_DRAG_STARTED: {
			onDragStarted((View) event.getLocalState(), v);
			break;
		}
		case DragEvent.ACTION_DRAG_ENTERED: {
			onDragEntered((View) event.getLocalState(), v);
			break;
		}
		case DragEvent.ACTION_DRAG_EXITED: {
			onDragExited((View) event.getLocalState(), v);
			break;
		}
		case DragEvent.ACTION_DROP: {
			View view = (View) event.getLocalState();
			onDragDropped(view, v);
			view.setVisibility(View.VISIBLE);
			break;
		}
		case DragEvent.ACTION_DRAG_ENDED: {
			View view = (View) event.getLocalState();
			onDragExited(view, v);
			view.setVisibility(View.VISIBLE);
		}
		default:
			break;
		}
		return true;
	}
}
