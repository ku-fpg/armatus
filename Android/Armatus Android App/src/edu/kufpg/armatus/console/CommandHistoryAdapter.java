package edu.kufpg.armatus.console;

import java.util.List;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.drag.DragIcon;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;

/**
 * {@link android.widget.ListAdapter ListAdapter} for a list of {@link DragIcon}s that
 * the user has most recently applied (i.e., the command history).
 */
public class CommandHistoryAdapter extends ArrayAdapter<String> {
	/** Reference to the current {@link Context}. */
	private Context mContext;
	
	/**
	 * Constructs a new instance with the specified command names.
	 * @param console The {@link ConsoleActivity} to use.
	 * @param commandNames The list of {@link DragIcon}s that the user has most recently
	 * used, from most recent to least recent.
	 */
	public CommandHistoryAdapter(Context context, List<String> commandNames) {
		super(context, R.layout.command_history_menu, commandNames);
		mContext = context;
	}
	
	@Override
	public View getView(int position, View convertView, ViewGroup parent){
		View entryView = convertView;
		CommandEntryHolder holder;
		if (entryView == null) {
			LayoutInflater inflater = LayoutInflater.from(mContext);
			entryView = inflater.inflate(R.layout.command_history_entry, parent, false);
			holder = new CommandEntryHolder();
			holder.icon = (DragIcon) entryView.findViewById(R.id.command_icon);
			entryView.setTag(holder);
		} else {
			holder = (CommandEntryHolder) entryView.getTag();
		}
		holder.icon.setText(getItem(position));
		holder.icon.setTypeface(ConsoleActivity.TYPEFACE);
		int newWidth = mContext.getResources().getDrawable(R.drawable.template_white).getIntrinsicWidth();
		holder.icon.getLayoutParams().width = newWidth;
		
		return entryView;
	}
	
	/** Holds {@link DragIcon} reference for efficiency purposes. */
	private static class CommandEntryHolder {
		public DragIcon icon;
	}

}
