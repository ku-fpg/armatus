package com.kufpg.armatus.dialog;

import com.kufpg.armatus.R;
import com.kufpg.armatus.console.ConsoleActivity;
import com.kufpg.armatus.console.PrettyPrinter;

import android.app.DialogFragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

public class ConsoleEntrySelectionDialog extends DialogFragment {

	private TextView mContentsView;
	private int mEntryNum;
	private String mEntryContents;
	
	public static ConsoleEntrySelectionDialog newInstance(int entryNum, String entryContents) {
		ConsoleEntrySelectionDialog cesd = new ConsoleEntrySelectionDialog();
		
		Bundle args = new Bundle();
		args.putInt("entryNum", entryNum);
		args.putString("entryContents", entryContents);
		cesd.setArguments(args);
		
		return cesd;
	}
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mEntryNum = getArguments().getInt("entryNum");
		mEntryContents = getArguments().getString("entryContents");
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.console_entry_selection_dialog, container, false);
		setCancelable(true);

		getDialog().setTitle("Entry number " + String.valueOf(mEntryNum));
		mContentsView = (TextView) v.findViewById(R.id.console_entry_selection_dialog_contents);
		mContentsView.setCursorVisible(true);
		mContentsView.setTypeface(ConsoleActivity.TYPEFACE);
		PrettyPrinter.setPrettyText(mContentsView, mEntryContents);
		
		return v;
	}

}
