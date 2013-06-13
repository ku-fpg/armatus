package com.kufpg.armatus.dialog;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;

public abstract class YesOrNoDialog extends DialogFragment {
	private String mTitle, mMessage;
	
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
			public void onClick(DialogInterface dialog, int whichButton) {
				yes(dialog, whichButton);
			}
		})
		.setNegativeButton("No", new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int whichButton) {
				no(dialog, whichButton);
			}
		})
		.create();
	}
	
	protected abstract void yes(DialogInterface dialog, int whichButton);
	protected void no(DialogInterface dialog, int whichButton) {}
	
	protected void setTitle(String title) {
		mTitle = title;
	}
	protected void setMessage(String message) {
		mMessage = message;
	}
}
