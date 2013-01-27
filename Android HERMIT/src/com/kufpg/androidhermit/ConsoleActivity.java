package com.kufpg.androidhermit;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map.Entry;

import com.kufpg.androidhermit.util.CommandDispatcher;
import com.kufpg.androidhermit.util.ConsoleTextView;

import android.content.Intent;
import android.graphics.Typeface;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.KeyEvent;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnLongClickListener;
import android.widget.ArrayAdapter;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.RelativeLayout.LayoutParams;
import android.widget.ScrollView;
import android.widget.TextView;

public class ConsoleActivity extends StandardActivity {

	private RelativeLayout mCodeLayout;
	private LayoutParams mCodeLayoutParams;
	private ScrollView mScrollView;
	private EditText mInputEditText;
	private TextView mInputHeader;
	private ConsoleTextView mCurConsoleTextView;
	private ConsoleTextView mPrevConsoleTextView = null;

	private LinkedHashMap<Integer, ConsoleTextView> mCommandHistory = new LinkedHashMap<Integer, ConsoleTextView>();
	private int mCommandCount = 0;
	private CommandDispatcher mDispatcher;
	private static final String RED = "red";
	private static final String BLUE = "blue";
	private static final String GREEN = "green";
	private HashMap<String,String> mContextMenuMap = new HashMap<String, String> ();
	private ArrayList<String> mValues = new ArrayList<String>();

	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console);

		mDispatcher = new CommandDispatcher(this);
		mScrollView = (ScrollView) findViewById(R.id.code_scroll_view);
		mCodeLayout = (RelativeLayout) findViewById(R.id.code_scroll_relative_layout);
		mInputHeader = (TextView) findViewById(R.id.code_command_num);
		mInputHeader.setText("hermit<" + mCommandCount + "> ");
		mInputEditText = (EditText) findViewById(R.id.code_input_box);
		mContextMenuMap.put(BLUE, BLUE);
		mContextMenuMap.put(RED, RED);
		mContextMenuMap.put(GREEN, GREEN);
		mInputEditText.setOnKeyListener(new EditText.OnKeyListener() {
			@Override
			public boolean onKey(View v, int keyCode, KeyEvent event) {
				if (keyCode == KeyEvent.KEYCODE_ENTER
						&& event.getAction() == KeyEvent.ACTION_UP) {
					String[] inputs = mInputEditText.getText().toString().split(" ");
					if(mDispatcher.isCommand(inputs[0])) {
						if(inputs.length == 1) {
							mDispatcher.execute(inputs[0]);
						} else {
							mDispatcher.execute(inputs[0], Arrays.copyOfRange
									(inputs, 1, inputs.length));
						}
					} else {
						addMessage(mInputEditText.getText().toString());
					}
					mInputEditText.setText(""); 
					return true;
				}
				return false;
			}
		});

		Typeface mTypeface = Typeface.createFromAsset(getAssets(), ConsoleTextView.TYPEFACE);
		mInputEditText.setTypeface(mTypeface);
		mInputHeader.setTypeface(mTypeface);
	}

	@Override
	public void onRestart() {
		super.onRestart();
		//Since onRestoreInstanceState() isn't called when
		//app sleeps or loses focus
		refreshConsole(mCommandHistory);
	}

	@Override
	public void onSaveInstanceState(Bundle savedInstanceState) {
		super.onSaveInstanceState(savedInstanceState);
		savedInstanceState.putInt("CmdCount", mCommandCount);
		savedInstanceState.putSerializable("CmdHistory", mCommandHistory);
		mCodeLayout.removeAllViews();
		mPrevConsoleTextView = null;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void onRestoreInstanceState(Bundle savedInstanceState) {
		super.onRestoreInstanceState(savedInstanceState);
		mCommandCount = savedInstanceState.getInt("CmdCount");
		mCommandHistory = (LinkedHashMap<Integer, ConsoleTextView>) savedInstanceState.getSerializable("CmdHistory");
		refreshConsole(mCommandHistory);
	}
	
	@Override
	public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo)
	{
		super.onCreateContextMenu(menu, v, menuInfo);
		menu.setHeaderTitle(R.string.Context_Menu_Title);
		int order = 0;
		for(String value: mValues)
		{
			menu.add(0, v.getId(), order, value);
			order++;
		}
	}
	
	@Override
	public boolean onContextItemSelected(MenuItem item)
	{
		if(item != null)
		{
			makeToast(item.getTitle().toString());
		}
		return super.onContextItemSelected(item);
	}

	public void clear() {
		mCodeLayout.removeAllViews();
		mCommandCount = 0;
		mCommandHistory.clear();
		mPrevConsoleTextView = null;
		mInputHeader.setText("hermit<" + mCommandCount + "> ");
	}
	
	public void exit() {
		this.finish();
		startActivity(new Intent(this, MainActivity.class));
	}

	/**
	 * Adds a new "line" to the console with msg as its contents.
	 * @param msg
	 */
	public void addMessage(String msg) {
		mCurConsoleTextView = new ConsoleTextView(ConsoleActivity.this, msg, mCommandCount);
		registerForContextMenu(mCurConsoleTextView);
		mCurConsoleTextView.setOnLongClickListener(new OnLongClickListener() {

			@Override
			public boolean onLongClick(View v) {
				
				mValues.clear();
				String [] mArry = mCurConsoleTextView.getText().toString().split(" ");
				
				for(int i = 1; i < mArry.length; i++)
				{
					if(mArry[i].equals(mContextMenuMap.get(BLUE)))
					{
						mValues.add("Blue");
					}
					else if(mArry[i].equals(mContextMenuMap.get(GREEN)))
					{
						mValues.add("Green");
					}
					else if(mArry[i].equals(mContextMenuMap.get(RED)))
					{
						mValues.add("Red");
					}
						
				}
				
				
				if(!mValues.isEmpty())
				{
					ConsoleActivity.this.openContextMenu(mCurConsoleTextView);
					return true;
				}
				
				return false;
			}
			
		});
		mCommandHistory.put(mCurConsoleTextView.getId(), mCurConsoleTextView);
		mCommandCount++;

		mCodeLayoutParams = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT,
				LayoutParams.WRAP_CONTENT);
		if (mPrevConsoleTextView != null)
			mCodeLayoutParams.addRule(RelativeLayout.BELOW, mPrevConsoleTextView.getId());
		mCodeLayout.addView(mCurConsoleTextView, mCodeLayoutParams);

		mScrollView.post(new Runnable() {
			public void run() {
				mScrollView.smoothScrollTo(0, mCurConsoleTextView.getBottom());
			}
		});
		mPrevConsoleTextView = mCurConsoleTextView;
		mInputHeader.setText("hermit<" + mCommandCount + "> ");
	}

	/**
	 * Similar to addMessage(String), but you can add an already built ConsoleTextView as an argument.
	 * Useful for when you have to rotate the screen and reconstruct the console buffer.
	 * @param ctv
	 */
	public void addTextView(final ConsoleTextView ctv) {
		if (!mCommandHistory.containsKey(ctv.getId())) {
			mCommandHistory.put(ctv.getId(), ctv);
			mCommandCount++;
		}

		mCodeLayoutParams = new RelativeLayout.LayoutParams(LayoutParams.WRAP_CONTENT,
				LayoutParams.WRAP_CONTENT);
		if (mPrevConsoleTextView != null)
			mCodeLayoutParams.addRule(RelativeLayout.BELOW, mPrevConsoleTextView.getId());
		mCodeLayout.addView(ctv, mCodeLayoutParams);

		mScrollView.post(new Runnable() {
			public void run() {
				mScrollView.smoothScrollTo(0, ctv.getBottom());
			}
		});
		mPrevConsoleTextView = ctv;
		mInputHeader.setText("hermit<" + mCommandCount + "> ");
	}

	/**
	 * Re-adds all of the ConsoleTextViews in conjunction with onRestart() and
	 * onRestoreInstanceState(Bundle).
	 * @param cmdHistory Pass as argument, since mCmdHistory could have been
	 * destroyed.
	 */
	private void refreshConsole(LinkedHashMap<Integer,ConsoleTextView> cmdHistory) {
		mCodeLayout.removeAllViews();
		mPrevConsoleTextView = null;
		for (Entry<Integer, ConsoleTextView> entry : cmdHistory.entrySet()) {
			addTextView(entry.getValue());
		}
	}

}
