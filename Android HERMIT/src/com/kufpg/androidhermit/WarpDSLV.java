package com.kufpg.androidhermit;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import com.kufpg.androidhermit.dragsort.DragSortListView;
import com.kufpg.androidhermit.util.FileIOManager;

import android.app.ActionBar;
import android.app.ListActivity;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.View.OnFocusChangeListener;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.EditText;

public class WarpDSLV extends ListActivity {

	public final static int FILE_FROM_DISK = 1;
	private CodeAdapter mAdapter;
	private ArrayList<String> mList;

	private DragSortListView.DropListener onDrop =
			new DragSortListView.DropListener() {
		@Override
		public void drop(int from, int to) {
			String item=mAdapter.getItem(from);

			mAdapter.notifyDataSetChanged();
			mAdapter.remove(item);
			mAdapter.insert(item, to);
		}
	};

	private DragSortListView.RemoveListener onRemove = 
			new DragSortListView.RemoveListener() {
		@Override
		public void remove(int which) {
			mAdapter.remove(mAdapter.getItem(which));
		}
	};

	private DragSortListView.DragScrollProfile ssProfile =
			new DragSortListView.DragScrollProfile() {
		@Override
		public float getSpeed(float w, long t) {
			if (w > 0.8f) {
				// Traverse all views in a millisecond
				return ((float) mAdapter.getCount()) / 0.001f;
			} else {
				return 10.0f * w;
			}
		}
	};

	/** Called when the activity is first created. */
	@SuppressWarnings("unchecked")
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.warp_main);
		ActionBar actionBar = getActionBar();
		actionBar.show();

		DragSortListView lv = (DragSortListView) getListView(); 

		lv.setDropListener(onDrop);
		lv.setRemoveListener(onRemove);
		lv.setDragScrollProfile(ssProfile);

		mList = (ArrayList<String>) getIntent().getSerializableExtra("CODE_ARRAY");

		mAdapter = new CodeAdapter(mList);
		setListAdapter(mAdapter);
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.actionbar, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item){
		switch(item.getItemId()) {
		case R.id.open_file:
			Intent filesIntent = new Intent();
			filesIntent.setType("text/plain");
			filesIntent.setAction(Intent.ACTION_GET_CONTENT);								
			startActivityForResult(Intent.createChooser(filesIntent,
					"Select app"), FILE_FROM_DISK);
			return true;
		case R.id.menu_settings:
			Intent settingsActivity = new Intent(getBaseContext(),
					Preferences.class);
			startActivity(settingsActivity);
			return true;
		default:
			return super.onOptionsItemSelected(item);
		}
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent intent) { 
		super.onActivityResult(requestCode, resultCode, intent);

		switch(requestCode) { 
		case FILE_FROM_DISK:
			if(resultCode == RESULT_OK) {
				Uri diskTextFile = intent.getData();
				ArrayList<String> code = null;
				try {
					code = FileIOManager.getTextArray(getContentResolver().openInputStream(diskTextFile));
				} catch (FileNotFoundException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				Intent codeIntent = new Intent(this, WarpDSLV.class);
				codeIntent.putExtra("CODE_ARRAY", code);
				finish();
				startActivity(codeIntent);
			}
		}
	}

	private class ViewHolder {
		public EditText codeView;
	}

	class ListItem {
		String lineOfCode;
	}

	private class CodeAdapter extends ArrayAdapter<String> {
		private LayoutInflater mInflater;
		private ArrayList<String> backupItems = new ArrayList<String>();

		public CodeAdapter(List<String> codeLines) {
			super(WarpDSLV.this, R.layout.list_item_handle_right,
					R.id.text, codeLines);
			mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			backupItems = mList;
			notifyDataSetChanged();
		}

		public int getCount() {
			return backupItems.size();
		}

		
		public View getView(int position, View convertView, ViewGroup parent) {
			ViewHolder holder;

			if (convertView == null) {
				holder = new ViewHolder();
				convertView = mInflater.inflate(R.layout.list_item_handle_right, null);
				holder.codeView = (EditText) convertView.findViewById(R.id.text);
				convertView.setTag(holder);
			} else {
				holder = (ViewHolder) convertView.getTag();
			}
			
			//Fill EditText with the value you have in data source
			holder.codeView.setText(backupItems.get(position));
			holder.codeView.setId(position);

			//we need to update adapter once we finish with editing
			holder.codeView.setOnFocusChangeListener(new OnFocusChangeListener() {
				public void onFocusChange(View v, boolean hasFocus) {
					if (!hasFocus){
						final int position = v.getId();
						final EditText Caption = (EditText) v;
						backupItems.set(position, Caption.getText().toString());
					}
				}
			});

			return convertView;
		}
	}

}
