package com.kufpg.armatus.console;

import java.util.List;

import com.kufpg.armatus.R;
import com.kufpg.armatus.drag.DragIcon;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;

public class CommandHistoryAdapter extends ArrayAdapter<String> {
	
	private ConsoleActivity mConsole;
	private List<String> mEntries;
	private CommandEntryHolder mHolder;
	
	public CommandHistoryAdapter(ConsoleActivity console, List<String> entries) {
		super(console, R.layout.command_history_menu, entries);
		mConsole = console;
		mEntries = entries;
	}
	
	public View getView(int position, View convertView, ViewGroup parent){
		View entryView = convertView;
		if (entryView == null) {
			LayoutInflater inflater = mConsole.getLayoutInflater();
			entryView = inflater.inflate(R.layout.command_history_entry, parent, false);
			mHolder = new CommandEntryHolder();
			mHolder.icon = (DragIcon) entryView.findViewById(R.id.command_icon);
			entryView.setTag(mHolder);
		} else {
			mHolder = (CommandEntryHolder) entryView.getTag();
		}
		mHolder.icon.setCommandName(mEntries.get(position));
		
		return entryView;
	}
	
	static class CommandEntryHolder {
		public DragIcon icon;
	}

}
