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
	
	public CommandHistoryAdapter(ConsoleActivity console, List<String> entries) {
		super(console, R.layout.command_history_menu, entries);
		mConsole = console;
	}
	
	public View getView(int position, View convertView, ViewGroup parent){
		View entryView = convertView;
		CommandEntryHolder holder;
		if (entryView == null) {
			LayoutInflater inflater = mConsole.getLayoutInflater();
			entryView = inflater.inflate(R.layout.command_history_entry, parent, false);
			holder = new CommandEntryHolder();
			holder.icon = (DragIcon) entryView.findViewById(R.id.command_icon);
			entryView.setTag(holder);
		} else {
			holder = (CommandEntryHolder) entryView.getTag();
		}
		holder.icon.setCommandName(getItem(position));
		
		return entryView;
	}
	
	private static class CommandEntryHolder {
		public DragIcon icon;
	}

}
