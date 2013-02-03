package com.kufpg.androidhermit;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import com.kufpg.androidhermit.util.FileIOUtils;
import com.kufpg.androidhermit.util.Tree;
import com.kufpg.androidhermit.util.Tree.TreeTraversalOrder;
import com.kufpg.androidhermit.util.TreeNode;
import com.slidingmenu.lib.SlidingMenu;

import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;
import android.widget.Toast;

public class MainActivity extends StandardActivity {

	private TextView mButtonsView;
	private Button mLockButton, mUnlockButton, mAsyncButton, mTreeButton,
			mConsoleButton, mPinchZoomButton;
	private CheckBox mProgressCheckBox;
	private int mNumTextChanges = 0;
	private boolean mIsLocked = false;
	private final ReentrantLock mLock = new ReentrantLock(true);
	private final Condition mLockInEffect = mLock.newCondition();
	private SlidingMenu mLeftSlidingMenu, mRightSlidingMenu;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_main);

		mButtonsView = (TextView) findViewById(R.id.code_text_view);
		setCodeText(mNumTextChanges, mIsLocked);
		mLockButton = (Button) findViewById(R.id.lock_button);
		mUnlockButton = (Button) findViewById(R.id.unlock_button);
		mAsyncButton = (Button) findViewById(R.id.async_button);
		mProgressCheckBox = (CheckBox) findViewById(R.id.progress_checkbox);
		mTreeButton = (Button) findViewById(R.id.tree_button);
		mConsoleButton = (Button) findViewById(R.id.console_button);
		mPinchZoomButton = (Button) findViewById(R.id.pinchzoom_button);

		mLockButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mLock.lock();
				try {
					while (mIsLocked) {
						try {
							mLockInEffect.await();
						} catch (InterruptedException e) {
							e.printStackTrace();
						}
					}
					mIsLocked = true;
					mLockButton.setEnabled(false);
					mNumTextChanges++;
					setCodeText(mNumTextChanges, mIsLocked);
				} finally {
					mLock.unlock();
				}
			}
		});

		mUnlockButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mLock.lock();
				try {
					if (mIsLocked) {
						mLockInEffect.signal();
						mIsLocked = false;
						mLockButton.setEnabled(true);
						setCodeText(mNumTextChanges, mIsLocked);
					}
				} finally {
					mLock.unlock();
				}
			}
		});

		mAsyncButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				Toast imgToast = new Toast(MainActivity.this);
				ImageView imgView = new ImageView(MainActivity.this);
				imgToast.setView(imgView);
				imgToast.setDuration(Toast.LENGTH_LONG);
				FileIOUtils
						.downloadImage(
								"http://3.bp.blogspot.com/-GYJu10jKqEw/Td2bEbUSzkI/AAAAAAAAAG0/m7t15oHOLWc/s1600/haskell-curry-says.png",
								imgView, mProgressCheckBox.isChecked(),
								MainActivity.this);
				imgToast.show();
			}
		});

		mTreeButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				Tree<Integer> testTree = new Tree<Integer>();
				TreeNode<Integer> rootNode = new TreeNode<Integer>(1);
				testTree.setRoot(rootNode);
				TreeNode<Integer> childNode = new TreeNode<Integer>(2);
				rootNode.addChild(childNode);
				rootNode.addChildren(3, 4, 5, 6);
				childNode.addChildren(7, 8, 9, 10);
				showToast(testTree.toString(TreeTraversalOrder.PRE_ORDER));
			}
		});

		mConsoleButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				Intent consoleIntent = new Intent(mContext,
						ConsoleActivity.class);
				startActivity(consoleIntent);
			}
		});
		
		mPinchZoomButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				Intent pinchZoomIntent = new Intent(mContext,
						TextSizePinchZoomActivity.class);
				startActivity(pinchZoomIntent);
			}
		});
		
		mLeftSlidingMenu = new SlidingMenu(this);
		mLeftSlidingMenu.setMode(SlidingMenu.LEFT_RIGHT);
		mLeftSlidingMenu.setTouchModeAbove(SlidingMenu.TOUCHMODE_FULLSCREEN);
		mLeftSlidingMenu.setFadeDegree(0.35f);
		mLeftSlidingMenu.setShadowWidthRes(R.dimen.shadow_width);
		mLeftSlidingMenu.setBehindOffsetRes(R.dimen.slidingmenu_offset);
		mLeftSlidingMenu.attachToActivity(this, SlidingMenu.SLIDING_CONTENT);
		mLeftSlidingMenu.setMenu(R.layout.drawer_menu);
		mLeftSlidingMenu.setSecondaryMenu(R.layout.drawer_menu);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		super.onCreateOptionsMenu(menu);
		return true;
	}

	private void setCodeText(int numTextChanges, boolean isLocked) {
		mButtonsView.setText("Button pushed " + numTextChanges
				+ " times. (Status: " + (isLocked ? "locked" : "unlocked")
				+ ".)");
	}

}
