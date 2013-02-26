package com.kufpg.androidhermit.drag;

import android.view.DragEvent;
import android.view.View;
import android.view.View.OnDragListener;

/**
 * Allows a View to listen for events that happen when a draggable View is brought
 * near, inside, or outside of it.
 */
public class DragSinkListener implements OnDragListener {
	
	/**
	 * Occurs when dragView begins a DragEvent.
	 * @param dragView The View being dragged.
	 * @param dragSink The View listening to dragView.
	 */
	public void onDragStarted(View dragView, View dragSink) {}
	
	/**
	 * Occurs when dragView is within the boundaries of dragSink.
	 * @param dragView The View being dragged.
	 * @param dragSink The View listening to dragView.
	 */
	public void onDragEntered(View dragView, View dragSink, DragEvent event) {}
	
	/**
	 * Occurs when dragView is within the boundaries of dragSink and is brought close
	 * to one of the boundaries.
	 * @param dragView The View being dragged.
	 * @param dragSink The View listening to dragView.
	 * @param event dragView's DragEvent.
	 */
	public void onDragNearBoundary(View dragView, View dragSink, DragEvent event) {}
	
	/**
	 * Occurs when dragView is brought outside the boundaries of dragSink.
	 * @param dragView The View being dragged.
	 * @param dragSink The View listening to dragView.
	 */
	public void onDragExited(View dragView, View dragSink) {}
	
	/**
	 * Occurs when dragView's DragEvent ends within the boundaries of dragSink.
	 * @param dragView The View being dragged.
	 * @param dragSink The View listening to dragView.
	 */
	public void onDragDropped(View dragView, View dragSink) {}
	
	/**
	 * Occurs when dragView's DragEvent ends, regardless of location.
	 * @param dragView The View being dragged.
	 * @param dragSink The View listening to dragView.
	 */
	public void onDragEnded(View dragView, View dragSink) {}

	@Override
	public boolean onDrag(View v, DragEvent event) {
		switch (event.getAction()) {
		case DragEvent.ACTION_DRAG_STARTED: {
			onDragStarted((View) event.getLocalState(), v);
			break;
		}
		case DragEvent.ACTION_DRAG_ENTERED: {
			onDragEntered((View) event.getLocalState(), v, event);
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
