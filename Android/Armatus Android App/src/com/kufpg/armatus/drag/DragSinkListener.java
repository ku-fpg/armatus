package com.kufpg.armatus.drag;

import android.view.DragEvent;
import android.view.View;
import android.view.View.OnDragListener;

/**
 * Allows a View to listen for events that happen when a draggable View is brought
 * near, inside, or outside of it.
 */
public class DragSinkListener implements OnDragListener {

	/**
	 * Occurs when dragSource begins a DragEvent.
	 * @param dragSource The View being dragged.
	 * @param dragSink The View listening to dragSource.
	 * @param event dragSource's DragEvent.
	 */
	public void onDragStarted(View dragSource, View dragSink, DragEvent event) {}

	/**
	 * Occurs when dragSource is within the boundaries of dragSink.
	 * @param dragSource The View being dragged.
	 * @param dragSink The View listening to dragSource.
	 * @param event dragSource's DragEvent.
	 */
	public void onDragEntered(View dragSource, View dragSink, DragEvent event) {}

	/**
	 * Occurs when dragSource is within the boundaries of dragSink.
	 * @param dragSource The View being dragged.
	 * @param dragSink The View listening to dragSource.
	 * @param event dragSource's DragEvent.
	 */
	public void onDragWithinBounds(View dragSource, View dragSink, DragEvent event) {}

	/**
	 * Occurs when dragSource is brought outside the boundaries of dragSink.
	 * @param dragSource The View being dragged.
	 * @param dragSink The View listening to dragSource.
	 * @param event dragSource's DragEvent.
	 */
	public void onDragExited(View dragSource, View dragSink, DragEvent event) {}

	/**
	 * Occurs when dragSource's DragEvent ends within the boundaries of dragSink.
	 * @param dragSource The View being dragged.
	 * @param dragSink The View listening to dragSource.
	 * @param event dragSource's DragEvent.
	 */
	public void onDragDropped(View dragSource, View dragSink, DragEvent event) {}

	/**
	 * Occurs when dragSource's DragEvent ends, regardless of location.
	 * @param dragSource The View being dragged.
	 * @param dragSink The View listening to dragSource.
	 * @param event dragSource's DragEvent.
	 */
	public void onDragEnded(View dragSource, View dragSink, DragEvent event) {}

	@Override
	public boolean onDrag(View v, DragEvent event) {
		switch (event.getAction()) {
		case DragEvent.ACTION_DRAG_STARTED: {
			onDragStarted((View) event.getLocalState(), v, event);
			break;
		}
		case DragEvent.ACTION_DRAG_ENTERED: {
			onDragEntered((View) event.getLocalState(), v, event);
			break;
		}
		case DragEvent.ACTION_DRAG_LOCATION: {
			onDragWithinBounds((View) event.getLocalState(), v, event);
			break;
		}
		case DragEvent.ACTION_DRAG_EXITED: {
			onDragExited((View) event.getLocalState(), v, event);
			break;
		}
		case DragEvent.ACTION_DROP: {
			onDragDropped((View) event.getLocalState(), v, event);
			break;
		}
		case DragEvent.ACTION_DRAG_ENDED: {
			onDragEnded((View) event.getLocalState(), v, event);
			break;
		}
		default:
			break;
		}
		return true;
	}
}
