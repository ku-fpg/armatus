package com.kufpg.androidhermit.console;

import java.util.List;

import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.console.ConsoleEntryAdapter.ConsoleEntryHolder;
import com.kufpg.androidhermit.drag.DragIcon;
import com.kufpg.androidhermit.drag.DragLayout;

import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;
import android.widget.Toast;

public class CommandHistoryAdapter extends ArrayAdapter<String> {
	
	private ConsoleActivity mConsole;
	private List<String> mEntries;
	private CommandEntryHolder mHolder;
	
	public CommandHistoryAdapter(ConsoleActivity console, List<String> hString)
	{
		super(console, R.layout.command_history, hString);
		mConsole = console;
		mEntries = hString;
	}
	
	public View getView(int position, View convertView, ViewGroup parent)
	{
		View entryView = convertView;
		if(entryView == null)
		{
			LayoutInflater inflater = mConsole.getLayoutInflater();
			entryView = inflater.inflate(R.layout.command_entry, parent, false);
			mHolder = new CommandEntryHolder();
			mHolder.cIcon = (DragIcon) entryView.findViewById(R.id.commandIcon);
			mHolder.cLayout = (DragLayout) entryView.findViewById(R.id.commandLayout);
			entryView.setTag(mHolder);
		}
		else
		{
			mHolder = (CommandEntryHolder) entryView.getTag();
		}
		mHolder.cIcon.setCommandName(mEntries.get(position));
		mHolder.cLayout.setSlidingMenu(mConsole.getSlidingMenu());
		
		return entryView;
	}
	
	static class CommandEntryHolder 
	{
		public DragIcon cIcon;
		public DragLayout cLayout;
	}

}
