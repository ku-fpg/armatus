package com.kufpg.androidhermit.dialog;

import com.kufpg.androidhermit.R;
import android.app.DialogFragment;
import android.os.Bundle;
import android.text.Html;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.EditText;

public class ConsoleEntrySelectionDialog extends DialogFragment {

	private EditText mContentsView;
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
		View v = inflater.inflate(R.layout.console_dialog, container, false);
		setCancelable(true);

		getDialog().setTitle("Entry number " + String.valueOf(mEntryNum));
		mContentsView = (EditText) v.findViewById(R.id.console_dialog_contents);
		mContentsView.setText(Html.fromHtml(mEntryContents)); //Don't forget, there could be special HTML formatting!
		
		return v;
	}

}
