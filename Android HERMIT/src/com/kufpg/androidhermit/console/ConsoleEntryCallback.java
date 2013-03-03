package com.kufpg.androidhermit.console;

import com.kufpg.androidhermit.R;

import android.content.Context;
import android.text.ClipboardManager;
import android.view.ActionMode;
import android.view.ActionMode.Callback;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;

public class ConsoleEntryCallback implements Callback {
	private ConsoleActivity mConsole;
	private ConsoleEntryAdapter mAdapter;
	private int mEntryNum;
	private String mEntryContents;
	
	public ConsoleEntryCallback(ConsoleActivity console, ConsoleEntryAdapter adapter,
			int entryNum, String entryContents) {
		mConsole = console;
		mAdapter = adapter;
		mEntryNum = entryNum;
		mEntryContents = entryContents;
	}
	
	@Override
	public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
		switch (item.getItemId()) {
		case R.id.copy:
			ClipboardManager clipboard = (ClipboardManager) mConsole.getSystemService(Context.CLIPBOARD_SERVICE); 
			clipboard.setText(mEntryContents);
			mConsole.showToast("Entry copied to clipboard!");
			mode.finish(); // Action picked, so close the CAB
			return true;
		case R.id.select:
			mConsole.showSelectionDialog(mEntryNum, mEntryContents);
			mode.finish();
			return true;
		}
		return false;
	}

	@Override
	public boolean onCreateActionMode(ActionMode mode, Menu menu) {
		MenuInflater inflater = mode.getMenuInflater();
		inflater.inflate(R.menu.contextual_action_bar, menu);
		return true;
	}

	@Override
	public void onDestroyActionMode(ActionMode mode) {
		mAdapter.deselectEntry();
	}

	@Override
	public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
		return false;
	}
}
