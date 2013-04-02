package com.kufpg.androidhermit.dialog;

import com.kufpg.androidhermit.R;

import android.app.AlertDialog;
import android.app.Dialog;
import android.app.DialogFragment;
import android.content.ActivityNotFoundException;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;

public class TerminalNotInstalledDialog extends DialogFragment {

	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState) {
		return new AlertDialog.Builder(getActivity())
		.setTitle(R.string.terminal_not_installed_title)
		.setMessage(R.string.terminal_not_installed_message)
		.setPositiveButton("Yes", new DialogInterface.OnClickListener() {
			public void onClick(DialogInterface dialog, int whichButton) {
				String appName="jackpal.androidterm";
				try {
				    startActivity(new Intent(Intent.ACTION_VIEW,
				    		Uri.parse("market://details?id="+appName)));
				} catch (ActivityNotFoundException anfe) {
				    startActivity(new Intent(Intent.ACTION_VIEW,
				    		Uri.parse("http://play.google.com/store/apps/details?id="+appName)));
				}
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
