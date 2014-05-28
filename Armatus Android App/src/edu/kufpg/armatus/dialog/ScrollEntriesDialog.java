package edu.kufpg.armatus.dialog;

import java.util.List;

import net.simonvt.numberpicker.NumberPicker;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleActivity;

public class ScrollEntriesDialog extends ConsiderateDialog {

	private int mEntryChoice;
	private String[] mEntries;
	
	public static ScrollEntriesDialog newInstance(int entryChoice, List<String> entries) {
		ScrollEntriesDialog sed = new ScrollEntriesDialog();
		Bundle args = new Bundle();
		args.putInt("entryChoice", entryChoice);
		String[] entriesArr = entries.toArray(new String[entries.size()]);
		entriesArr[entriesArr.length - 1] = "<no input>";
		args.putStringArray("entries", entriesArr);
		sed.setArguments(args);
		return sed;
	}
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mEntryChoice = getArguments().getInt("entryChoice");
		mEntries = getArguments().getStringArray("entries");
	}
	
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.console_scroll_entries_dialog, container, false);
		getDialog().setTitle("User input history selection");
		setCancelable(true);
		
		final NumberPicker picker = (NumberPicker) v.findViewById(R.id.console_scroll_entries_picker);
		picker.setDescendantFocusability(ViewGroup.FOCUS_BLOCK_DESCENDANTS);
		picker.setMinValue(0);
		picker.setMaxValue(mEntries.length - 1);
		picker.setDisplayedValues(mEntries);
		picker.setValue(mEntryChoice);
		picker.setTypeface(ConsoleActivity.TYPEFACE);
		picker.setWrapSelectorWheel(false);
		
		Button confirm = (Button) v.findViewById(R.id.console_scroll_entries_confirm);
		confirm.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				getConsole().selectFromUserInputHistory(picker.getValue());
				dismiss();
			}
		});
		
		return v;
	}
	
}
