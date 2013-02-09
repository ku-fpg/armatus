package com.kufpg.androidhermit.util.drag;

import android.view.DragEvent;
import android.view.View;
import android.view.View.OnDragListener;

public class DragSinkListener implements OnDragListener {
	public void onDragStarted(View dragView, View dragSink) {}
	public void onDragEntered(View dragView, View dragSink) {}
	public void onDragNearBoundary(View dragView, View dragSink, DragEvent event) {}
	public void onDragExited(View dragView, View dragSink) {}
	public void onDragDropped(View dragView, View dragSink) {}
	public void onDragEnded(View dragView, View dragSink) {}

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
		case DragEvent.ACTION_DRAG_LOCATION: {
			onDragNearBoundary((View) event.getLocalState(), v, event);
			break;
		}
		case DragEvent.ACTION_DRAG_EXITED: {
			onDragExited((View) event.getLocalState(), v);
			break;
		}
		case DragEvent.ACTION_DROP: {
			View dragView = (View) event.getLocalState();
			onDragDropped(dragView, v);
			dragView.setVisibility(View.VISIBLE);
			break;
		}
		case DragEvent.ACTION_DRAG_ENDED: {
			View dragView = (View) event.getLocalState();
			onDragEnded(dragView, v);
			dragView.setVisibility(View.VISIBLE);
		}
		default:
			break;
		}
		return true;
	}
}
