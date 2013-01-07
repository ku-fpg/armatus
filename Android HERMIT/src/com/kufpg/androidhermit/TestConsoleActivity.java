package com.kufpg.androidhermit;

import android.os.Bundle;
import android.view.View;
import android.widget.Button;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.TextView;

public class TestConsoleActivity extends StandardActivity {

	private RelativeLayout rr;
	private Button b1;
	private TextView tv1;
	private View recent;
	private LayoutParams lp;

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.test_console);

		rr = (RelativeLayout) findViewById(R.id.whatever);
		b1 = (Button) findViewById(R.id.add_text_button);
		recent = b1;
		b1.setText("Click me");

		b1.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				tv1 = new TextView(TestConsoleActivity.this);
				tv1.setId((int) System.currentTimeMillis());
				lp = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT,
						LayoutParams.WRAP_CONTENT);
				lp.addRule(RelativeLayout.BELOW, recent.getId());
				tv1.setText("Time: " + System.currentTimeMillis());
				rr.addView(tv1, lp);
				recent = tv1;
			}
		});
	}

}
