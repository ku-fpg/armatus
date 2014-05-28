package edu.kufpg.armatus.console;

import java.util.List;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.data.HistoryCommand;

public class CommandHistoryAdapter extends ArrayAdapter<HistoryCommand> {
	
	private Context mContext;
	
	public CommandHistoryAdapter(Context context, List<HistoryCommand> history) {
		super(context, R.layout.command_history_entry, history);
		mContext = context;
	}
	
	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		CommandHistoryViewHolder holder;
		
		if (convertView == null) {
			LayoutInflater inflater = LayoutInflater.from(mContext);
			convertView = inflater.inflate(R.layout.command_history_entry, parent, false);
			holder = new CommandHistoryViewHolder();
			holder.fromAst = (TextView) convertView.findViewById(R.id.command_history_ast_from);
			holder.contents = (TextView) convertView.findViewById(R.id.command_history_contents);
			convertView.setTag(holder);
		} else {
			holder = (CommandHistoryViewHolder) convertView.getTag();
		}
		
		HistoryCommand command = getItem(position);
		holder.fromAst.setText(String.valueOf(command.getFrom()));
		holder.contents.setText(command.getCommand());
		holder.contents.setTypeface(ConsoleActivity.TYPEFACE);
		return convertView;
	}
	
	private static class CommandHistoryViewHolder {
		TextView fromAst, contents;
	}

}
