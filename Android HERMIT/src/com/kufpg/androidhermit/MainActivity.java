package com.kufpg.androidhermit;

import android.os.Bundle;
import android.view.Menu;
import android.widget.ScrollView;
import android.widget.TextView;

public class MainActivity extends StandardActivity {

	private ScrollView scrollView;
	private TextView codeView;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		scrollView = (ScrollView) findViewById(R.id.code_scroll_view);
		codeView = (TextView) findViewById(R.id.code_text_view);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);
		return true;
	}

	@Override
	public void openCode(String code) {
		codeView.setText(code);
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putIntArray("ARTICLE_SCROLL_POSITION",
				new int[]{ scrollView.getScrollX(), scrollView.getScrollY()});
	}

	@Override
	protected void onRestoreInstanceState(Bundle savedInstanceState) {
		super.onRestoreInstanceState(savedInstanceState);
		final int[] position = savedInstanceState.getIntArray("ARTICLE_SCROLL_POSITION");
		if(position != null)
			scrollView.post(new Runnable() {
				public void run() {
					scrollView.scrollTo(position[0], position[1]);
				}
			});
	}

}
