package edu.kufpg.armatus.console;

import java.util.List;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.drag.DragIcon;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;

/**
 * {@link android.widget.ListAdapter ListAdapter} for a list of {@link DragIcon}s that
 * the user has most recently applied (i.e., the command history).
 */
public class CommandHistoryAdapter extends ArrayAdapter<String> {
	/** Reference to the current console. */
	private ConsoleActivity mConsole;
	
	/**
	 * Constructs a new instance with the specified command names.
	 * @param console The {@link ConsoleActivity} to use.
	 * @param commandNames The list of {@link DragIcon}s that the user has most recently
	 * used, from most recent to least recent.
	 */
	public CommandHistoryAdapter(ConsoleActivity console, List<String> commandNames) {
		super(console, R.layout.command_history_menu, commandNames);
		mConsole = console;
	}
	
	@Override
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
	
	/** Holds {@link DragIcon} reference for efficiency purposes. */
	private static class CommandEntryHolder {
		public DragIcon icon;
	}

}
