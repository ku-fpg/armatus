package edu.kufpg.armatus.util;

import android.text.style.ClickableSpan;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnTouchListener;

public abstract class TouchableSpan extends ClickableSpan implements OnTouchListener {
	
	@Override
	public boolean onTouch(View v, MotionEvent event) {
		return false;
	}

}
