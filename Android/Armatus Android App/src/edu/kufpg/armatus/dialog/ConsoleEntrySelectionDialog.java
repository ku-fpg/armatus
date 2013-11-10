package edu.kufpg.armatus.dialog;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.util.StringUtils;

import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.ClipboardManager.OnPrimaryClipChangedListener;
import android.content.Context;
import android.os.Bundle;
import android.text.SpannableStringBuilder;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

/**
 * ConsoleEntrySelectionDialog Class. This class extends {@link android.app.DialogFragment DialogFragment} class. 
 * Task of building strings and making the dialog of the console. 
 */
public class ConsoleEntrySelectionDialog extends ConsiderateDialog {
	private ClipboardManager mClipboard;
	private TextView mContentsView;
	private OnPrimaryClipChangedListener mClipboardUnwrapper;
	private int mFirstEntryNum;
	private CharSequence mContents;

	public static ConsoleEntrySelectionDialog newInstance(int... entryNums) {
		ConsoleEntrySelectionDialog cesd = new ConsoleEntrySelectionDialog();

		Bundle args = new Bundle();
		if (entryNums.length == 1) {
			args.putInt("firstEntryNum", entryNums[0]);
		} else {
			args.putInt("firstEntryNum", -1);
		}
		args.putIntArray("entryNums", entryNums);
		cesd.setArguments(args);

		return cesd;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		if (savedInstanceState == null) {
			mFirstEntryNum = getArguments().getInt("firstEntryNum");
			SpannableStringBuilder contentsBuilder = new SpannableStringBuilder();
			for (int entryNum : getArguments().getIntArray("entryNums")) {
				ConsoleEntry entry = getConsole().getEntry(entryNum);
				contentsBuilder.append(entry.getFullContents()).append('\n');
			}
			contentsBuilder.delete(contentsBuilder.length() - 1, contentsBuilder.length()); //Remove final newline
			mContents = contentsBuilder;
		} else {
			
			mContents = savedInstanceState.getCharSequence("contents");
		}

		mClipboard = (ClipboardManager) getActivity().getSystemService(Context.CLIPBOARD_SERVICE);
		mClipboardUnwrapper = new OnPrimaryClipChangedListener() {
			@Override
			public void onPrimaryClipChanged() {
				if (mClipboard.hasPrimaryClip() && mClipboard.getPrimaryClipDescription().hasMimeType("text/plain")) {
					mClipboard.removePrimaryClipChangedListener(this);
					String contents = mClipboard.getPrimaryClip().getItemAt(0).getText().toString();
					ClipData newCopy = ClipData.newPlainText("copiedText", StringUtils.noCharWrap(contents));
					mClipboard.setPrimaryClip(newCopy);
					mClipboard.addPrimaryClipChangedListener(this);
				}
			}
		};
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.console_entry_selection_dialog, container, false);
		setCancelable(true);

		if (mFirstEntryNum > -1) { //If there is only one entry
			getDialog().setTitle("Entry number " + mFirstEntryNum);
		} else {
			getDialog().setTitle("Selected entries");
		}
		mContentsView = (TextView) v.findViewById(R.id.console_entry_selection_dialog_contents);
		mContentsView.setText(mContents);
		mContentsView.setTypeface(ConsoleActivity.TYPEFACE);

		return v;
	}

	@Override
	public void onResume() {
		super.onResume();
		mClipboard.addPrimaryClipChangedListener(mClipboardUnwrapper);
	}

	@Override
	public void onPause() {
		super.onPause();
		mClipboard.removePrimaryClipChangedListener(mClipboardUnwrapper);
	}
	
	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putCharSequence("contents", mContents);
	}
	

}