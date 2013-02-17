package com.kufpg.androidhermit.console;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.drag.CommandIcon;
import com.kufpg.androidhermit.drag.DragSinkListener;

import android.content.Context;
import android.graphics.Color;
import android.graphics.Typeface;
import android.text.Editable;
import android.text.Html;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.MotionEvent;
import android.view.View;
import android.widget.TextView;

public class ConsoleTextView extends TextView implements Serializable {

	private static final long serialVersionUID = 492620301229198361L;
	private static final int PADDING = ConsoleActivity.PADDING;

	private String mMessage = "";
	private int mCommandNum;
	private List<String> mKeywords = new ArrayList<String>();
	//Don't try to serialize a ConsoleActivity; you're in for pain
	private transient ConsoleActivity mConsole;

	public ConsoleTextView(Context context) {
		super(context);
	}

	public ConsoleTextView(Context context, AttributeSet attrs) {
		super(context, attrs);
	}

	public ConsoleTextView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);
	}

	public ConsoleTextView(ConsoleActivity console, String msg, int commandNum) {
		super(console);
		setupView(console, msg, commandNum);
	}

	protected void setupView(ConsoleActivity console, String msg, int commandNum) {
		mConsole = console;
		mCommandNum = commandNum;
		Typeface typeface = Typeface.createFromAsset(getResources().getAssets(),
				ConsoleActivity.TYPEFACE);

		addTextChangedListener(new PrettyPrinter());
		setGravity(Gravity.BOTTOM);
		//TODO: Better ID system
		setId((int) System.currentTimeMillis());
		setOnDragListener(new DragSinkListener() {
			@Override
			public void onDragEntered(View dragView, View dragSink) {
				setBackground(getResources().getDrawable(R.drawable.console_text_border));
			}

			@Override
			public void onDragExited(View dragView, View dragSink) {
				setBackgroundColor(getResources().getColor(android.R.color.transparent));
			}

			@Override
			public void onDragEnded(View dragView, View dragSink) {
				setBackgroundColor(getResources().getColor(android.R.color.transparent));
			}

			@Override
			public void onDragDropped(View dragView, View dragSink) {
				if (!mKeywords.isEmpty()) {
					mConsole.setTempCommand(((CommandIcon) dragView).getCommandName());
					mConsole.setTempKeywords(mKeywords);
					mConsole.openContextMenu(dragSink);
				}
			}
		});
		setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				if (!mKeywords.isEmpty()) {
					mConsole.setTempCommand(null);
					return true;
				}
				mConsole.setTempKeywords(mKeywords);
				mConsole.openContextMenu(v);
				return false;
			}
		});
		setPaddingRelative(PADDING, PADDING, PADDING, 0);
		setTextColor(Color.WHITE);
		setTextSize(ConsoleActivity.DEFAULT_FONT_SIZE);
		setTypeface(typeface);

		setText("hermit<" + commandNum + "> ");		
		if (msg != null) {
			mMessage = msg;
			append(msg);
			String[] inputArr = msg.split(ConsoleActivity.WHITESPACE);
			for(String word : inputArr) {
				if(CommandDispatcher.isKeyword(word)) {
					mKeywords.add(word);
				}
			}
		}
	}

	public String getMessage() {
		return mMessage;
	}
	
	public int getCommandNum() {
		return mCommandNum;
	}

	public List<String> getKeywords() {
		return mKeywords;
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {
		if(event.getAction() == MotionEvent.ACTION_DOWN) {
			setBackgroundResource(R.drawable.console_text_border);
		} else if(event.getAction() == MotionEvent.ACTION_UP
				|| event.getAction() == MotionEvent.ACTION_OUTSIDE
				|| event.getAction() == MotionEvent.ACTION_CANCEL) {
			setBackgroundResource(android.R.color.transparent);
		}
		return super.onTouchEvent(event);	
	}

	public class PrettyPrinter implements TextWatcher {
		public static final String RED = "#CC060B";
		public static final String GREEN = "#1DDA1C";
		public static final String BLUE = "#0090D3";
		private String lastText = null;

		@Override
		public void onTextChanged(CharSequence s, int start,
				int before, int count) {
			if (!s.toString().equals(lastText)) {
				lastText = s.toString();

				String res = "";
				//Make sure to sanitize string for HTML parsing
				String[] sentence = TextUtils.htmlEncode(s.toString())
						.split(ConsoleActivity.WHITESPACE);
				for (String word : sentence) {
					String color = null;
					if (CommandDispatcher.isKeyword(word)) {
						color = CommandDispatcher.getKeyword(word).getColor();
						res += "<font color='" + color + "'>" +	word + "</font> ";
					} else {
						res += word + " ";
					}
				};
				ConsoleTextView.this.setText(Html.fromHtml(res));
			}
		}

		@Override
		public void beforeTextChanged(CharSequence s, int start, int count, int after) {}

		@Override
		public void afterTextChanged(Editable s) {}
	}

}