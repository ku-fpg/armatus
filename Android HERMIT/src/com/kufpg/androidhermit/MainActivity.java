package com.kufpg.androidhermit;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;

import com.kufpg.androidhermit.util.FileIOManager;
import com.kufpg.androidhermit.util.Tree;
import com.kufpg.androidhermit.util.Tree.TreeTraversalOrder;
import com.kufpg.androidhermit.util.TreeNode;

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

	private TextView codeView;
	private Button lockButton, unlockButton, asyncButton, treeButton;
	private CheckBox progressCheckBox;
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
		asyncButton = (Button) findViewById(R.id.async_button);
		treeButton = (Button) findViewById(R.id.tree_button);
		progressCheckBox = (CheckBox) findViewById(R.id.progress_checkbox);

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
		
		asyncButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				Toast imgToast = new Toast(MainActivity.this);
	            ImageView imgView = new ImageView(MainActivity.this);     
	            imgToast.setView(imgView);
	            imgToast.setDuration(Toast.LENGTH_LONG);
	            FileIOManager.downloadImage("http://3.bp.blogspot.com/-GYJu10jKqEw/Td2bEbUSzkI/AAAAAAAAAG0/m7t15oHOLWc/s1600/haskell-curry-says.png",
	            		imgView, progressCheckBox.isChecked(), MainActivity.this);
	            imgToast.show();
			}
		});
		
		treeButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View arg0) {
				Tree<Integer> testTree = new Tree<Integer>();
				TreeNode<Integer> rootNode = new TreeNode<Integer>(1);
				testTree.setRoot(rootNode);
				TreeNode<Integer> childNode = new TreeNode<Integer>(2);
				rootNode.addChild(childNode);
				rootNode.addChildren(3, 4, 5, 6);
				childNode.addChildren(7, 8, 9, 10);
				makeToast(testTree.toString(TreeTraversalOrder.PRE_ORDER));
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
