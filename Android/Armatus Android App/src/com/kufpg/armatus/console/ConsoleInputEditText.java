package com.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;

import android.content.Context;
import android.text.Editable;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.TextWatcher;
import android.text.style.LeadingMarginSpan;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.widget.EditText;

public class ConsoleInputEditText extends EditText implements TextWatcher {
	private static final KeyEvent Q = new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_Q);
	private static final KeyEvent DEL = new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL);
	private int mChangedStartIndex, mChangedEndIndex;
	private List<SpanParams> mIndentSpans = new ArrayList<SpanParams>();

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

	public void setTextSpan(Object what, int start, int end, int flags) {
		Spannable spannable = new SpannableString(getText());
		removeTextChangedListener(this);
		if (what instanceof LeadingMarginSpan) {
			mIndentSpans.add(new SpanParams(what, start, end, flags));
		}
		spannable.setSpan(what, start, end, flags);
		setText(spannable);
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
		int cursorPos = getSelectionStart();
		
		//Remove additional spans that might be introduced through pasting
		for (LeadingMarginSpan span : s.getSpans(0, s.length(), LeadingMarginSpan.class)) {
			s.removeSpan(span);
		}
		for (SpanParams param : mIndentSpans) {
			s.setSpan(param.what, param.start, param.end, param.flags);
		}

		removeTextChangedListener(this);
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

	private class SpanParams {
		public final Object what;
		public final int start, end, flags;

		public SpanParams(Object what, int start, int end, int flags) {
			this.what = what;
			this.start = start;
			this.end = end;
			this.flags = flags;
		}
	}

}
