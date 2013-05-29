package com.kufpg.armatus.console;

import com.kufpg.armatus.R;

import android.content.ClipData;
import android.content.ClipboardManager;
import android.content.Context;
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
			ClipData copiedText = ClipData.newPlainText("copiedText", mEntryContents);
			clipboard.setPrimaryClip(copiedText);
			mConsole.showToast("Entry copied to clipboard!");
			mode.finish(); // Action picked, so close the CAB
			return true;
		case R.id.select:
			mConsole.showEntryDialog(mEntryNum, mEntryContents, ConsoleActivity.SELECTION_TAG);
			mode.finish();
			return true;
		case R.id.swap:
			mConsole.showEntryDialog(mEntryNum, mEntryContents, ConsoleActivity.KEYWORD_SWAP_TAG);
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
