package com.kufpg.androidhermit;

import android.os.Bundle;
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
	private TextView tv1;
	private View recent;
	private LayoutParams lp;
	private ScrollView sv;
	private EditText et;

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

	private void addMessage(String msg) {
		tv1 = new TextView(TestConsoleActivity.this);
		tv1.setGravity(Gravity.BOTTOM);
		tv1.setId((int) System.currentTimeMillis());
		lp = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT,
				LayoutParams.WRAP_CONTENT);
		lp.addRule(RelativeLayout.BELOW, recent.getId());
		tv1.setText(msg);
		rr.addView(tv1, lp);
		sv.post(new Runnable() {
			public void run() {
				sv.smoothScrollTo(0, tv1.getBottom());
			}
		});
		recent = tv1;
	}

}
