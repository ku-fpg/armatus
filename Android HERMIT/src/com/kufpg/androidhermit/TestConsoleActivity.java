package com.kufpg.androidhermit;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map.Entry;

import android.os.Bundle;
import android.util.SparseArray;
import android.view.Gravity;
import android.view.KeyEvent;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.ScrollView;
import android.widget.TextView;

public class TestConsoleActivity extends StandardActivity {

	private RelativeLayout rr;
	private Button b1;
	private View recent;
	private LayoutParams lp;
	private ScrollView sv;
	private EditText et;

	private HashMap<Integer, TextView> cmdHistory = new HashMap<Integer, TextView>();
	private TextView tv;

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.test_console);

		sv = (ScrollView) findViewById(R.id.scroll);
		rr = (RelativeLayout) findViewById(R.id.whatever);
		et = (EditText) findViewById(R.id.inputBox);
		et.setOnKeyListener(new EditText.OnKeyListener() {
			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP) {
					addMessage("Time: " + System.currentTimeMillis());
					return true;
				}
				return false;
			}

		});

		b1 = (Button) findViewById(R.id.add_text_button);
		recent = b1;
		b1.setText("Click me");
		b1.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				addMessage("Time: " + System.currentTimeMillis());
			}
		});
	}

	@Override
	public void onSaveInstanceState(Bundle savedInstanceState) {
		super.onSaveInstanceState(savedInstanceState);
		savedInstanceState.putSerializable("cmdHistory", cmdHistory);
		Iterator<Entry<Integer, TextView>> cmdIter = cmdHistory.entrySet()
				.iterator();
		while (cmdIter.hasNext()) {
			Entry<Integer, TextView> curEntry = (Entry<Integer, TextView>) cmdIter
					.next();
			rr.removeView(curEntry.getValue());
		}
	}

	@Override
	public void onRestoreInstanceState(Bundle savedInstanceState) {
		super.onRestoreInstanceState(savedInstanceState);
		cmdHistory = (HashMap<Integer, TextView>) savedInstanceState
				.getSerializable("cmdHistory");
		Iterator<Entry<Integer, TextView>> cmdIter = cmdHistory.entrySet()
				.iterator();
		while (cmdIter.hasNext()) {
			Entry<Integer, TextView> curEntry = (Entry<Integer, TextView>) cmdIter
					.next();
			addTextView(curEntry.getValue());
		}
	}

	/**
	 * Adds a new "line" to the console with msg as its contents.
	 */
	private void addMessage(String msg) {
		tv = new TextView(TestConsoleActivity.this);
		tv.setGravity(Gravity.BOTTOM);
		// TODO: Make a better ID system
		tv.setId((int) System.currentTimeMillis());
		tv.setText(msg);
		cmdHistory.put(tv.getId(), tv);

		lp = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT,
				LayoutParams.WRAP_CONTENT);
		lp.addRule(RelativeLayout.BELOW, recent.getId());
		rr.addView(tv, lp);

		sv.post(new Runnable() {
			public void run() {
				sv.smoothScrollTo(0, tv.getBottom());
			}
		});
		recent = tv;
	}

	private void addTextView(final TextView textView) {
		if (!cmdHistory.containsKey(textView.getId())) {
			cmdHistory.put(textView.getId(), textView);
		}

		lp = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT,
				LayoutParams.WRAP_CONTENT);
		lp.addRule(RelativeLayout.BELOW, recent.getId());
		rr.addView(textView, lp);

		sv.post(new Runnable() {
			public void run() {
				sv.smoothScrollTo(0, textView.getBottom());
			}
		});
		recent = textView;
	}

}
