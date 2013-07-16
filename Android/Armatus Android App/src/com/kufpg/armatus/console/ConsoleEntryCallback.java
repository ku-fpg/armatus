package com.kufpg.armatus.console;

import com.kufpg.armatus.R;
import com.kufpg.armatus.util.StringUtils;

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
	private ConsoleListView mListView;
	private boolean mIsVisible = false;

	public ConsoleEntryCallback(ConsoleActivity console) {
		mConsole = console;
		mListView = (ConsoleListView) console.getListView();
	}

	@Override
	public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
		ConsoleEntry entry = (ConsoleEntry) mListView.getItemAtPosition(mListView.getCheckedItemPosition());
		switch (item.getItemId()) {
		case R.id.copy:
			ClipboardManager clipboard = (ClipboardManager) mConsole.getSystemService(Context.CLIPBOARD_SERVICE); 
			ClipData copiedText = ClipData.newPlainText("copiedText",
					StringUtils.removeCharWrap(entry.getFullContents()));
			clipboard.setPrimaryClip(copiedText);
			mConsole.showToast("Entry copied to clipboard!");
			mode.finish(); // Action picked, so close the contextual action bar
			return true;
		case R.id.select:
			mConsole.showEntryDialog(entry.getNum(), entry.getFullContents(), ConsoleActivity.SELECTION_TAG);
			mode.finish();
			return true;
		case R.id.swap:
			mConsole.showEntryDialog(entry.getNum(), entry.getShortContents(), ConsoleActivity.KEYWORD_SWAP_TAG);
			mode.finish();
			return true;
		}
		return false;
	}

	@Override
	public boolean onCreateActionMode(ActionMode mode, Menu menu) {
		MenuInflater inflater = mode.getMenuInflater();
		inflater.inflate(R.menu.contextual_action_bar, menu);
		mIsVisible = true;
		return true;
	}

	@Override
	public void onDestroyActionMode(ActionMode mode) {
		mListView.resetCheckedPos();
		mIsVisible = false;
	}

	@Override
	public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
		return false;
	}
	
	public boolean isVisible() {
		return mIsVisible;
	}
}
