package edu.kufpg.armatus.drag;

import edu.kufpg.armatus.console.HermitClient.CommandInfo;
import android.content.ClipData;
import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.TextView;

/**
 * A draggable image associated with a {@link edu.kufpg.armatus.console.CustomCommandInfo.Command
 * Command} that can be run on {@link edu.kufpg.armatus.console.ConsoleEntry ConsoleEntry} {@link
 * edu.kufpg.armatus.command.CustomCommandDispatcher.Keyword Keywords}.
 */
public class DragIcon extends TextView {
	private CommandInfo mCommandInfo;
	
	public DragIcon(Context context) {
		super(context);
		init();
	}

	public DragIcon(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public DragIcon(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}
	
	private void init() {
		setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				ClipData dragData = ClipData.newPlainText("", "");
				DragShadowBuilder builder = new DragShadowBuilder(v);
				v.startDrag(dragData, builder, v, 0);
				v.setVisibility(View.INVISIBLE);
				return true;
			}
		});
	}
	
	public CommandInfo getCommandInfo() {
		return mCommandInfo;
	}
	
	public void setCommandInfo(CommandInfo commandInfo) {
		mCommandInfo = commandInfo;
		setText(commandInfo.getName());
	}
}
