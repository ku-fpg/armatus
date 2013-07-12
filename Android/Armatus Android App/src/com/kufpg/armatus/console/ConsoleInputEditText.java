package com.kufpg.armatus.console;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.widget.EditText;

public class ConsoleInputEditText extends EditText implements TextWatcher {

	public ConsoleInputEditText(Context context) {
		super(context);
		init();
	}

	public ConsoleInputEditText(Context context, AttributeSet attrs) {
		super(context, attrs);
		init();
	}

	public ConsoleInputEditText(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
		init();
	}
	
	private void init() {
		addTextChangedListener(this);
	}

	//	@Override
	//	public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
	//		InputConnection conn = super.onCreateInputConnection(outAttrs);
	//		outAttrs.imeOptions &= ~EditorInfo.IME_FLAG_NO_ENTER_ACTION;
	//		return conn;
	//	}

	@Override
	public void afterTextChanged(Editable s) {
		int index = s.toString().indexOf(" ");
		if (index != -1) {
			s.replace(index, index+1, "\u00A0");
		}
	}

	@Override
	public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

	@Override
	public void onTextChanged (CharSequence s, int start, int before, int count) {}

}
