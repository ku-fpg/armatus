package edu.kufpg.armatus.drag;

import java.util.Locale;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.console.CommandDispatcher;
import edu.kufpg.armatus.console.CommandDispatcher.Command;

import android.content.ClipData;
import android.content.Context;
import android.util.AttributeSet;
import android.view.View;
import android.widget.ImageView;
import android.widget.TextView;

/**
 * A drag able image that represents a Command that can be run on console entry Keywords. It extends {@link android.widget.ImageView ImageView}
 */
public class DragIcon extends TextView {
	//private String mCommandName; 
	//private String mCommandImagePath;

	/**
	 * DragIcon Function for selected icon
	 * @param {@link android.content.Context context}
	 */
	public DragIcon(Context context) {
		this(context, null);
	}

	/**
	 * DragIcon with context and an attribute set within icon
	 * @param {@link android.content.Context context}
	 * @param {@link android.util.AttributeSet attrs}
	 */
	public DragIcon(Context context, AttributeSet attrs) {
		this(context, attrs, 0);
	}

	/**
	 * DragIcon function that allows drag after a long click that is triggered by listener.
	 * Uses elements
	 * @param {@link android.content.Context context}
	 * @param {@link android.util.AttributeSet attrs}
	 * @param defStyle
	 */
	public DragIcon(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
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

	/**
	 * Function to check to see if the command has a icon. 
	 * Works with {@link CommandDispatcher.Command Command}
	 * @param {@link android.content.Context context}
	 * @param {@link java.lang.String commandName}
	 * @return
	 */
	public static boolean commandHasIcon(Context context, String commandName) {
		Command command = CommandDispatcher.getCommand(commandName);
		if (commandName == null || command == null) {
			return false;
		}
		String groupName = command.getGroupName();
		if (groupName == null) {
			return false;
		}
		groupName = groupName.replaceAll("[/ ]", "_").toLowerCase(Locale.US);
		
		String pathCommand = new String(commandName);
		if (command.getCommandAlias() != null) {
			pathCommand = command.getCommandAlias();
		}
		String path = "command_" + groupName + "_" + pathCommand.replace("-", "").toLowerCase(Locale.US);
		int resid = context.getResources().getIdentifier(path, "drawable", BaseActivity.PACKAGE_NAME);
		return resid != 0;
	}
}
