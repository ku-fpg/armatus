package com.kufpg.androidhermit.drag;

import com.kufpg.androidhermit.R;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.widget.ImageView;

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

		setOnLongClickListener(new DragSourceClickListener());
		TypedArray ta = context.obtainStyledAttributes(attrs, R.styleable.DragImageView);
		mCommandName = ta.getString(R.styleable.DragImageView_command);
		if (mCommandName == null) {
			mCommandName = "toast";
		}
	}

	public String getCommandName() {
		return mCommandName;
	}
}
