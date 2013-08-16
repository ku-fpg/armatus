package edu.kufpg.armatus.dialog;

import edu.kufpg.armatus.console.ConsoleActivity;
import android.app.DialogFragment;
import android.content.DialogInterface;
import android.os.Bundle;

public class ConsiderateDialog extends DialogFragment {
	private ConsoleActivity mConsole;
	private boolean mWasSoftKeyboardCollapsed = false;
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mConsole = (ConsoleActivity) getActivity();
		if (savedInstanceState == null) {
			if (mConsole.isSoftKeyboardVisible()) {
				mWasSoftKeyboardCollapsed = true;
				mConsole.setSoftKeyboardVisibility(false);
			}
		} else {
			mWasSoftKeyboardCollapsed = savedInstanceState.getBoolean("keyboard");
		}
	}
	
	@Override
	public void onDismiss(DialogInterface dialog) {
		super.onDismiss(dialog);
		if (mWasSoftKeyboardCollapsed) {
			mConsole.setSoftKeyboardVisibility(true);
		} else {
			mConsole.setSoftKeyboardVisibility(false);
		}
	}
	
	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putBoolean("keyboard", mWasSoftKeyboardCollapsed);
	}
	
	protected ConsoleActivity getConsole() {
		return mConsole;
	}

}
