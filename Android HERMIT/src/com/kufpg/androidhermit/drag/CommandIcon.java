package com.kufpg.androidhermit.drag;

import com.kufpg.androidhermit.R;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.widget.ImageView;

/**
 * A draggable image that represents a Command that can be run on console entry Keywords.
 */
public class CommandIcon extends ImageView {
	private String mCommandName = null;
	
	public CommandIcon(Context context) {
		this(context, null);
	}

	public CommandIcon(Context context, AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public CommandIcon(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);

		setOnLongClickListener(new DragViewClickListener());
		TypedArray ta = context.obtainStyledAttributes(attrs, R.styleable.DragImageView);
		mCommandName = ta.getString(R.styleable.DragImageView_command);
		if (mCommandName == null) {
			mCommandName = "toast";
		}
		ta.recycle();
	}

	public String getCommandName() {
		return mCommandName;
	}
}
