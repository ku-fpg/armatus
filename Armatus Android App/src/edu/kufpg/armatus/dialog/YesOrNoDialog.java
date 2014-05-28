package edu.kufpg.armatus.dialog;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;

/**
 * Abstract YesOrNoDialog class, this extends the {@link android.app.DialogFragment DialogFragment} class. 
 * Class that makes a title and message of yes or no.
 */
public abstract class YesOrNoDialog extends DialogFragment {
	private String mTitle, mMessage;
	
	/**
	 * YesOrNoDialog Function, this put intial values to the mTitle and mMessage variables that is read in from the params.
	 * @param title
	 * @param message
	 */
	public YesOrNoDialog(String title, String message) {
		mTitle = title;
		mMessage = message;
	}
	
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState) {
		return new AlertDialog.Builder(getActivity())
		.setTitle(mTitle)
		.setMessage(mMessage)
		.setPositiveButton("Yes", new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int whichButton) {
				yes(dialog, whichButton);
			}
		})
		.setNegativeButton("No", new DialogInterface.OnClickListener() {
			@Override
			public void onClick(DialogInterface dialog, int whichButton) {
				no(dialog, whichButton);
			}
		})
		.create();
	}
	
	/**
	 * Abstract void Yes Function
	 * @param {@link android.content.DialogInterface dialog}
	 * @param whichButton
	 */
	protected abstract void yes(DialogInterface dialog, int whichButton);
	
	/**
	 * Void no Function
	 * @param {@link android.content.DialogInterface dialog}
	 * @param whichButton
	 */
	protected void no(DialogInterface dialog, int whichButton) {}
	
	/**
	 * Void setTitle Function. Sets the mTitle variable to the param title
	 * @param {@link java.lang.String title}
	 */
	protected void setTitle(String title) {
		mTitle = title;
	}
	
	/**
	 * Void setMessage Function. Sets the mMessage variable to the param message.
	 * @param {@link java.lang.String message}
	 */
	protected void setMessage(String message) {
		mMessage = message;
	}
}
