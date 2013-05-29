package com.kufpg.armatus.drag;

import java.util.Locale;

import com.kufpg.armatus.console.CommandDispatcher;
import com.kufpg.armatus.console.CommandDispatcher.Command;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.ImageView;

/**
 * A draggable image that represents a Command that can be run on console entry Keywords.
 */
public class DragIcon extends ImageView {
	private String mCommandName, mCommandImagePath;

	public DragIcon(Context context) {
		this(context, null);
	}

	public DragIcon(Context context, AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public DragIcon(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);

		setOnLongClickListener(new DragViewClickListener());
	}

	public String getCommandName() {
		return mCommandName;
	}

	public void setCommandName(String commandName) {
		Command command = CommandDispatcher.getCommand(commandName);
		if (commandName == null || command == null) {
			mCommandName = "toast"; //Because toast is delicious
		} else if (commandName.equals("translate-rewrite")) {
			mCommandName = "<+";
		} else if (commandName.equals("rewrites")) {
			mCommandName = ">>>";
		} else if (commandName.equals("rewrites-one-fail")) {
			mCommandName = ">+>";
		} else {
			mCommandName = commandName;
		}
		
		String groupName = CommandDispatcher.getCommand(mCommandName).getGroupName();	
		groupName = groupName.replaceAll("[/ ]", "_").toLowerCase(Locale.US);
		mCommandImagePath = "command_" + groupName + "_" + commandName.replace("-", "").toLowerCase(Locale.US);

		int resid = getResources().getIdentifier(mCommandImagePath, "drawable", "com.kufpg.armatus");
		if (resid != 0) {
			setBackground(getResources().getDrawable(resid));
		}
	}
}
