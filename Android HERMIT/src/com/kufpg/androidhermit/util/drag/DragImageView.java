package com.kufpg.androidhermit.util.drag;

import android.content.Context;
import android.util.AttributeSet;
import android.widget.ImageView;

public class DragImageView extends ImageView {
	
	public DragImageView(Context context) {
		super(context);
		setupView();
	}

	public DragImageView(Context context, AttributeSet attrs) {
		super(context, attrs);
		setupView();
	}

	public DragImageView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		setupView();
	}
	
	private void setupView() {
		setOnLongClickListener(new DragSourceClickListener());
	}

}
