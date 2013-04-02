package com.kufpg.androidhermit.dialog;

import java.util.ArrayList;

import com.kufpg.androidhermit.R;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.gesture.Gesture;
import android.gesture.GestureLibraries;
import android.gesture.GestureLibrary;
import android.gesture.GestureOverlayView;
import android.gesture.GestureOverlayView.OnGesturePerformedListener;
import android.gesture.Prediction;
import android.graphics.Rect;
import android.os.Bundle;
import android.view.Gravity;
import android.view.Window;
import android.widget.TextView;
import android.widget.Toast;

public class GestureDialog extends DialogFragment implements OnGesturePerformedListener {

	private GestureLibrary gestureLib;

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState) {
		// retrieve display dimensions
		Rect displayRectangle = new Rect();
		Window window = getActivity().getWindow();
		window.getDecorView().getWindowVisibleDisplayFrame(displayRectangle);

		TextView tv = new TextView(getActivity());
		tv.setGravity(Gravity.CENTER);
		tv.setMinimumWidth((int)(displayRectangle.width() * 0.9f));
		tv.setMinimumHeight((int)(displayRectangle.height() * 0.9f));
		tv.setText(R.string.gesture_dialog_message);

		GestureOverlayView gestureOverlayView = new GestureOverlayView(getActivity());
		gestureOverlayView.addView(tv);
		gestureOverlayView.addOnGesturePerformedListener(this);
		gestureLib = GestureLibraries.fromRawResource(getActivity(), R.raw.gestures);
		if (!gestureLib.load()) {
			//Exception?
		}

		return new AlertDialog.Builder(getActivity())
		.setView(gestureOverlayView)
		.setNegativeButton("Never mind", new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int whichButton) {
				//Do nothing
			}
		})
		.create();
	}

	@Override
	public void onGesturePerformed(GestureOverlayView overlay, Gesture gesture) {
		ArrayList<Prediction> predictions = gestureLib.recognize(gesture);
		for (Prediction prediction : predictions) {
			if (prediction.score > 1.0) {
				Toast.makeText(getActivity(), prediction.name, Toast.LENGTH_SHORT)
				.show();
			}
		}
	}

}
