package com.kufpg.androidhermit;

import java.io.FileNotFoundException;
import java.util.ArrayList;

import com.kufpg.androidhermit.dragsort.DragSortListView;
import com.kufpg.androidhermit.util.FileIOManager;


import android.app.ActionBar;
import android.app.ListActivity;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.widget.ArrayAdapter;

public class WarpDSLV extends ListActivity {

	public final static int FILE_FROM_DISK = 1;
    private ArrayAdapter<String> adapter;
    private ArrayList<String> list;

    private DragSortListView.DropListener onDrop =
        new DragSortListView.DropListener() {
            @Override
            public void drop(int from, int to) {
                String item=adapter.getItem(from);

                adapter.notifyDataSetChanged();
                adapter.remove(item);
                adapter.insert(item, to);
            }
        };

    private DragSortListView.RemoveListener onRemove = 
        new DragSortListView.RemoveListener() {
            @Override
            public void remove(int which) {
                adapter.remove(adapter.getItem(which));
            }
        };

    private DragSortListView.DragScrollProfile ssProfile =
        new DragSortListView.DragScrollProfile() {
            @Override
            public float getSpeed(float w, long t) {
                if (w > 0.8f) {
                    // Traverse all views in a millisecond
                    return ((float) adapter.getCount()) / 0.001f;
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

        list = (ArrayList<String>) getIntent().getSerializableExtra("CODE_ARRAY");

        adapter = new ArrayAdapter<String>(this, R.layout.list_item_handle_right, R.id.text, list);
        setListAdapter(adapter);
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

}
