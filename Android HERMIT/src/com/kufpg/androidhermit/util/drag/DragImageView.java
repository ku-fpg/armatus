package com.kufpg.androidhermit.util.drag;

import com.kufpg.androidhermit.R;

import android.content.Context;
import android.content.res.TypedArray;
import android.util.AttributeSet;
import android.widget.ImageView;

public class DragImageView extends ImageView {
	private String mCommandName = null;
	
	public DragImageView(Context context) {
		super(context, null);
	}

	public DragImageView(Context context, AttributeSet attrs) {
		super(context, attrs, 0);
	}

	public DragImageView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);

		setOnLongClickListener(new DragSourceClickListener());
/*		TypedArray ta = context.obtainStyledAttributes(attrs, R.styleable.DragImageView);
		mCommandName = ta.getString(R.styleable.DragImageView_command);
		if (mCommandName == null) {
			mCommandName = "toast";
		}*/
	}

	public String getCommandName() {
		return mCommandName;
	}
}
