package edu.kufpg.armatus.util;

import android.text.style.ClickableSpan;
import android.view.View;

public abstract class LongClickableSpan extends ClickableSpan {

	@Override
	public void onClick(View widget) {}
	
	public abstract void onLongClick(View widget);

}
