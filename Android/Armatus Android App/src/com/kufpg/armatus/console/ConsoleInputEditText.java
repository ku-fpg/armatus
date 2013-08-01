package com.kufpg.armatus.console;

import com.kufpg.armatus.util.StringUtils;

import android.content.Context;
import android.text.Editable;
import android.text.TextWatcher;
import android.text.style.LeadingMarginSpan;
import android.util.AttributeSet;
import android.view.KeyEvent;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputConnection;
import android.widget.EditText;

/**
 * An {@link EditText} that has two unique properties:
 * <ol>
 * <li>The first line of a <code>ConsoleInputEditText</code> is indented. This is indended
 * to allow for a custom prompt (e.g., <code>armatus@android~$</code>) to be placed in the
 * space left by the indentation.</code></li>
 * <li>This class does not use the default word-wrapping behavior of {@link
 * android.widget.TextView TextView}; instead, <code>ConsoleInputEdiText</code> wraps by
 * character to more closely resemble an actual terminal.</li>
 * </ol>
 */
public class ConsoleInputEditText extends EditText {
	/**
	 * The Q key isn't important in itself; I just need to type some character in for
	 * a workaround to fix {@link LeadingMarginSpan} issues.
	 */
	private static final KeyEvent Q = new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_Q);
	
	/**
	 * The delete key, for immediately deleting the "Q" that I type in a workaround to fix
	 * some annoying {@link LeadingMarginSpan} issues.
	 */
	private static final KeyEvent DEL = new KeyEvent(KeyEvent.ACTION_DOWN, KeyEvent.KEYCODE_DEL);
	
	/** The span which defines this {@link android.widget.TextView TextView}'s indent amount. */
	private LeadingMarginSpan.Standard mIndent;

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
			/** Tracks the beginning index (inclusive) of the most recent text change. */
			private int mChangedStartIndex;
			
			/** Tracks the end index (exclusive) of the most recent text change. */
			private int mChangedEndIndex;
			
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
						s.replace(mChangedStartIndex, mChangedStartIndex+1, StringUtils.NBSP);
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
	
	/**
	 * Sets the length of the indentation. Note that if you are using a custom prompt,
	 * the length of the indentation should be the length of the prompt minus the prompt's
	 * padding.
	 * @param length of the indentation.
	 */
	public void setIndent(int length) {
		//First remove the original indent(s)
		for (LeadingMarginSpan span : getText().getSpans(0, getText().length(), LeadingMarginSpan.class)) {
			getText().removeSpan(span);
		}
		mIndent = new LeadingMarginSpan.Standard(length, 0);
		getText().setSpan(mIndent, 0, 0, 0);
	}

	@Override
	public InputConnection onCreateInputConnection(EditorInfo outAttrs) {
		//Hack to allow for entry submission when the Enter key is pushed.
		InputConnection conn = super.onCreateInputConnection(outAttrs);
		outAttrs.imeOptions &= ~EditorInfo.IME_FLAG_NO_ENTER_ACTION;
		return conn;
	}

}
