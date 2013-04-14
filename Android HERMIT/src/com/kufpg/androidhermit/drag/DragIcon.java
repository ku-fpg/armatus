package com.kufpg.androidhermit.drag;

import com.kufpg.androidhermit.R;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.widget.ImageView;

/**
 * A draggable image that represents a Command that can be run on console entry Keywords.
 */
public class DragIcon extends ImageView {
	private String mCommandName = null;

	public DragIcon(Context context) {
		this(context, null);
	}

	public DragIcon(Context context, AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public DragIcon(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);

		setOnLongClickListener(new DragViewClickListener());
		TypedArray ta = context.obtainStyledAttributes(attrs, R.styleable.DragIcon);
		setCommandName(ta.getString(R.styleable.DragIcon_command));
		ta.recycle();
	}

	public String getCommandName() {
		return mCommandName;
	}

	public void setCommandName(String commandName) {
		if (commandName == null) {
			mCommandName = "toast"; //Because toast is delicious
		} else {
			mCommandName = commandName;
		}

		int resid = getResources().getIdentifier(mCommandName, "drawable", "com.kufpg.androidhermit");
		if (resid != 0) {
			setBackground(getResources().getDrawable(resid));
		}
	}
}
