package com.kufpg.androidhermit;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

public class MainActivity extends StandardActivity {

	private TextView codeView;
	private Button lockButton, unlockButton;
	private int mNumTextChanges = 0;
	private boolean mIsLocked = false;
	private final ReentrantLock mLock = new ReentrantLock(true);
	private final Condition lockInEffect = mLock.newCondition();

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		codeView = (TextView) findViewById(R.id.code_text_view);
		setCodeText(mNumTextChanges, mIsLocked);
		lockButton = (Button) findViewById(R.id.lock_button);
		unlockButton = (Button) findViewById(R.id.unlock_button);

		lockButton.setOnClickListener(new OnClickListener() {	
			@Override
			public void onClick(View v) {
				mLock.lock();
				try {
					while(mIsLocked) {
						try {
							lockInEffect.await();
						} catch (InterruptedException e) {
							e.printStackTrace();
						}
					}
					mIsLocked = true;
					lockButton.setEnabled(false);
					mNumTextChanges++;
					setCodeText(mNumTextChanges, mIsLocked);
				} finally {
					mLock.unlock();
				}
			}
		});

		unlockButton.setOnClickListener(new OnClickListener() {	
			@Override
			public void onClick(View v) {
				mLock.lock();
				try {
					if(mIsLocked) {
						lockInEffect.signal();
						mIsLocked = false;
						lockButton.setEnabled(true);
						setCodeText(mNumTextChanges, mIsLocked);
					}
				} finally {
					mLock.unlock();
				}
			}
		});
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);
		return true;
	}

	private void setCodeText(int numTextChanges, boolean isLocked) {
		codeView.setText("Button pushed " + numTextChanges + " times. (Status: " + (isLocked ? "locked" : "unlocked") + ".)");
	}

}
