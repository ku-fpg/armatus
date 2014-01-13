package edu.kufpg.armatus.dialog;

import java.util.ArrayList;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.gesture.Gesture;
import android.gesture.GestureLibraries;
import android.gesture.GestureLibrary;
import android.gesture.GestureOverlayView;
import android.gesture.GestureOverlayView.OnGesturePerformedListener;
import android.gesture.Prediction;
import android.graphics.Point;
import android.os.Bundle;
import android.util.Log;
import android.view.Display;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;
import android.view.ViewGroup.LayoutParams;
import android.widget.TextView;
import edu.kufpg.armatus.R;

/**
 * GestureDialog Class for the various gestures. Extends the {@link android.app.DialogFragment DialogFragment} and 
 * implements the {@link android.gesture.GestureOverLayView.OnGesturePerfomedListner OnGesturePerformedListner}. 
 */
public class GestureDialog extends ConsiderateDialog implements OnGesturePerformedListener {

	private GestureLibrary mGestureLib;
	private GestureOverlayView mGestureView;

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState) {
		TextView tv = new TextView(getActivity());
		tv.setGravity(Gravity.CENTER);
		tv.setText(R.string.gesture_dialog_message);

		mGestureView = new GestureOverlayView(getActivity());
		resizeGestureView();
		mGestureView.addView(tv, new LayoutParams(LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT));
		mGestureView.addOnGesturePerformedListener(this);
		mGestureLib = GestureLibraries.fromRawResource(getActivity(), R.raw.gestures);
		if (!mGestureLib.load()) {
			//Exception?
		}
		mGestureView.setOnTouchListener(new OnTouchListener() {
			@Override
			public boolean onTouch(View v, MotionEvent event) {
				switch (event.getAction() & MotionEvent.ACTION_MASK) {
				case MotionEvent.ACTION_POINTER_DOWN:
					mGestureView.cancelGesture();
					mGestureView.cancelClearAnimation();
					Log.d("TESTTESTTEST", "Two fingers whoa!!!");
					return true;
				}
				return false;
			}
		});

		return new AlertDialog.Builder(getActivity())
		.setView(mGestureView)
		.setNegativeButton("Never mind", new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int whichButton) {}
		}).create();
	}

	@Override
	public void onGesturePerformed(GestureOverlayView overlay, Gesture gesture) {
		ArrayList<Prediction> predictions = mGestureLib.recognize(gesture);
		if (!predictions.isEmpty()) {
			Prediction bestPrediction = predictions.get(0);
			double maxScore = predictions.get(0).score;
			for (Prediction prediction : predictions) {
				if (prediction.score > maxScore) {
					bestPrediction = prediction;
					maxScore = prediction.score;
				}
			}
			getConsole().showToast(bestPrediction.name);
		}
	}

	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		resizeGestureView();
	}
	
	/**
	 * Private resizeGestureView function
	 * Looks at the size of the window and adjusts the gesture to the screen.
	 */
	private void resizeGestureView() {
		Display display = getActivity().getWindowManager().getDefaultDisplay();
		Point size = new Point();
		display.getSize(size);
		mGestureView.setMinimumHeight((int)(size.x * 0.9));
		mGestureView.setMinimumWidth((int)(size.y * 0.9));
	}

}
