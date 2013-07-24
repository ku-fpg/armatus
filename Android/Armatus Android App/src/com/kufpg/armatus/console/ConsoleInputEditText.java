package com.kufpg.armatus.console;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.text.style.LeadingMarginSpan;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.widget.EditText;

public class ConsoleInputEditText extends EditText {
	private static final KeyEvent Q = new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_Q);
	private static final KeyEvent DEL = new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL);
	private LeadingMarginSpan.Standard mIndent;
	private int mChangedStartIndex, mChangedEndIndex;

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
		mIndent = new LeadingMarginSpan.Standard(0, 0);
		getText().setSpan(mIndent, 0, 0, 0);
		addTextChangedListener(new TextWatcher() {
			@Override
			public void afterTextChanged(Editable s) {
				int cursorPos = getSelectionStart();

				removeTextChangedListener(this);
				//Remove additional spans that might be introduced through pasting
				for (LeadingMarginSpan span : s.getSpans(0, s.length(), LeadingMarginSpan.class)) {
					s.removeSpan(span);
				}
				s.setSpan(mIndent, 0, 0, 0);
				
				for (; mChangedStartIndex < mChangedEndIndex; mChangedStartIndex++) {
					switch (s.charAt(mChangedStartIndex)) {
					case ' ':
						//Prevent TextView's default word-wrapping behavior (wrap by character instead)
						s.replace(mChangedStartIndex, mChangedStartIndex+1, "\u00A0");
						break;
					case '\n':
						/* A strange bug exists where pasting multiple lines will seemingly indent all
						 * newlines--until the text is changed via typing. Therefore, this will initiate
						 * the typing on each newline to prevent weirdness. */
						setSelection(mChangedStartIndex);
						dispatchKeyEvent(Q);
						dispatchKeyEvent(DEL);
						break;
					}
				}
				addTextChangedListener(this);
				setSelection(cursorPos);
			}

			@Override
			public void beforeTextChanged(CharSequence s, int start, int count, int after) {
				mChangedStartIndex = start;
				mChangedEndIndex = start + after;
			}

			@Override
			public void onTextChanged(CharSequence s, int start, int before, int count) {}
		});
	}
	
	public void setIndent(int length) {
		for (LeadingMarginSpan span : getText().getSpans(0, getText().length(), LeadingMarginSpan.class)) {
			getText().removeSpan(span);
		}
		mIndent = new LeadingMarginSpan.Standard(length, 0);
		getText().setSpan(mIndent, 0, 0, 0);
	}

	@Override
	public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
		InputConnection conn = super.onCreateInputConnection(outAttrs);
		outAttrs.imeOptions &= ~EditorInfo.IME_FLAG_NO_ENTER_ACTION;
		return conn;
	}

}
