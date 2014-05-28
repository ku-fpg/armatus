package edu.kufpg.armatus.dialog;

import edu.kufpg.armatus.R;

import android.app.Dialog;
import android.content.ActivityNotFoundException;
import android.content.DialogInterface;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;

/**
 * TermianlNotInstallDialog Class, this class extends the {@link edu.kufpg.armatus.dialog.YesOrNoDialog YesOrNoDialog} class.
 * This class will tell if a console application is installed or not.
 */
public class TerminalNotInstalledDialog extends YesOrNoDialog {
	public TerminalNotInstalledDialog() {
		super(null, null);
	}
	
	@Override
	public Dialog onCreateDialog(Bundle savedInstanceState) {
		setTitle(getResources().getString(R.string.terminal_not_installed_title));
		setMessage(getResources().getString(R.string.terminal_not_installed_message));
		return super.onCreateDialog(savedInstanceState);
	}

	@Override
	protected void yes(DialogInterface dialog, int whichButton) {
		String appName = "jackpal.androidterm";
		try {
			startActivity(new Intent(Intent.ACTION_VIEW,
					Uri.parse("market://details?id="+appName)));
		} catch (ActivityNotFoundException anfe) {
			startActivity(new Intent(Intent.ACTION_VIEW,
					Uri.parse("http://play.google.com/store/apps/details?id="+appName)));
		}
	}
}
