package com.kufpg.armatus.dialog;

import com.kufpg.armatus.R;
import com.kufpg.armatus.console.ConsoleActivity;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;

public class ConsoleExitDialog extends DialogFragment {

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState) {
		return new AlertDialog.Builder(getActivity())
		.setTitle(R.string.console_exit_title)
		.setMessage(R.string.console_exit_message)
		.setPositiveButton("Yes", new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int whichButton) {
				((ConsoleActivity) getActivity()).exit();
			}
		})
		.setNegativeButton("No", new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int whichButton) {
				//Do nothing
			}
		})
		.create();
	}

}
