package com.kufpg.androidhermit;

import java.io.FileNotFoundException;
import java.util.ArrayList;
import java.util.List;

import com.kufpg.androidhermit.dragsort.DragSortListView;
import com.kufpg.androidhermit.util.FileIOUtils;
import android.app.ActionBar;
import android.app.AlertDialog;
import android.app.ListActivity;
import android.content.Context;
import android.content.DialogInterface;
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
import android.widget.Toast;

public class WarpDSLV extends ListActivity {

	public final static int FILE_FROM_DISK = 1;
	private Context mContext;
	private CodeAdapter mAdapter;
	private ArrayList<String> mList;
	private String mFileName;

	private DragSortListView.DropListener onDrop = new DragSortListView.DropListener() {
		@Override
		public void drop(int from, int to) {
			String item = mAdapter.getItem(from);

			mAdapter.notifyDataSetChanged();
			mAdapter.remove(item);
			mAdapter.insert(item, to);
		}
	};

	private DragSortListView.RemoveListener onRemove = new DragSortListView.RemoveListener() {
		@Override
		public void remove(int which) {
			mAdapter.remove(mAdapter.getItem(which));
		}
	};

	private DragSortListView.DragScrollProfile ssProfile = new DragSortListView.DragScrollProfile() {
		@Override
		public float getSpeed(float w, long t) {
			if (w > 0.8f) {
				// Traverse all views in a millisecond
				return (mAdapter.getCount()) / 0.001f;
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

		mFileName = getIntent().getStringExtra("CODE_FILENAME");
		if (getIntent().getStringExtra("CODE_PATH") != null)
			StandardActivity
					.setSaveDir(getIntent().getStringExtra("CODE_PATH"));
		mList = (ArrayList<String>) getIntent().getSerializableExtra(
				"CODE_ARRAY");
		mAdapter = new CodeAdapter(mList);
		setListAdapter(mAdapter);
		mContext = getApplicationContext();
	}

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.actionbar, menu);
		return true;
	}

	@Override
	public boolean onOptionsItemSelected(MenuItem item) {
		switch (item.getItemId()) {
		case R.id.open_from_disk:
			Intent filesIntent = new Intent();
			filesIntent.setType("text/plain");
			filesIntent.setAction(Intent.ACTION_GET_CONTENT);
			startActivityForResult(
					Intent.createChooser(filesIntent, "Select app"),
					FILE_FROM_DISK);
			return true;
		case R.id.open_from_url:
			final Context c = this;
			AlertDialog.Builder alert = new AlertDialog.Builder(c);
			alert.setMessage(R.string.open_from_url_message);

			// Set an EditText view to get user input
			final EditText inputBox = new EditText(c);
			alert.setView(inputBox);

			alert.setPositiveButton("Open",
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog,
								int whichButton) {
							String textInput = inputBox.getText().toString();
							if (FileIOUtils.isTextFile(textInput)) {
								ArrayList<String> code = FileIOUtils
										.getTextArrayFromUrl(textInput);
								Intent codeIntent = new Intent(mContext,
										WarpDSLV.class);
								codeIntent.putExtra("CODE_ARRAY", code);
								String[] uriBits = textInput.split("/");
								codeIntent.putExtra("CODE_FILENAME",
										uriBits[uriBits.length - 1]);
								finish();
								startActivity(codeIntent);
							} else {
								makeToast("The entered URL is not a plaintext file.");
							}
						}
					});

			alert.setNegativeButton("Cancel",
					new DialogInterface.OnClickListener() {
						@Override
						public void onClick(DialogInterface dialog,
								int whichButton) {
							// Cancelled.
						}
					});

			alert.show();
			return true;
		case R.id.save_file:
			if (FileIOUtils.saveTextArray(mList,
					StandardActivity.getSaveDir(), mFileName)) {
				makeToast(mFileName + " saved successfully!");
			} else {
				makeToast("There was an error saving " + mFileName);
			}
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

	public void makeToast(String message) {
		Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
	}

	@Override
	protected void onActivityResult(int requestCode, int resultCode,
			Intent intent) {
		super.onActivityResult(requestCode, resultCode, intent);

		switch (requestCode) {
		case FILE_FROM_DISK:
			if (resultCode == RESULT_OK) {
				Uri diskTextFile = intent.getData();
				ArrayList<String> code = null;
				try {
					code = FileIOUtils
							.getTextArrayFromDisk(getContentResolver()
									.openInputStream(diskTextFile));
				} catch (FileNotFoundException e) {
					e.printStackTrace();
				}
				Intent codeIntent = new Intent(this, WarpDSLV.class);
				codeIntent.putExtra("CODE_ARRAY", code);
				String[] uriBits = intent.getDataString().split("/");
				String codePath = "";
				for (int i = 0; i < uriBits.length - 1; i++)
					codePath += "/" + uriBits[i];
				codeIntent.putExtra("CODE_PATH", codePath);
				codeIntent.putExtra("CODE_FILENAME",
						uriBits[uriBits.length - 1]);

				/* Code for opening a file with HermitJsonObject */
				// Uri diskTextFile = intent.getData();
				// HermitJsonObject hjo = null;
				// try {
				// hjo = new
				// HermitJsonObject(getContentResolver().openInputStream(diskTextFile));
				// } catch (FileNotFoundException e) {
				// e.printStackTrace();
				// } catch (JSONException e) {
				// e.printStackTrace();
				// }
				// Intent codeIntent = new Intent(this, WarpDSLV.class);
				// codeIntent.putExtra("CODE_ARRAY", hjo.getJSONContents());
				// String[] uriBits = intent.getDataString().replace("file:///",
				// "").split("/"); //The replace method is due to the Nexus 7's
				// filesystem, so this may need to be improved later
				// String codePath = "";
				// for(int i = 0; i < uriBits.length - 1; i++)
				// codePath += "/" + uriBits[i];
				// codeIntent.putExtra("CODE_PATH", codePath);
				// codeIntent.putExtra("CODE_FILENAME",
				// uriBits[uriBits.length-1]);

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
		private ArrayList<String> backupList = new ArrayList<String>();

		public CodeAdapter(List<String> codeLines) {
			super(WarpDSLV.this, R.layout.list_item_handle_right, R.id.text,
					codeLines);
			mInflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
			backupList = mList;
			notifyDataSetChanged();
		}

		@Override
		public int getCount() {
			return backupList.size();
		}

		@Override
		public View getView(int position, View convertView, ViewGroup parent) {
			ViewHolder holder;

			if (convertView == null) {
				holder = new ViewHolder();
				convertView = mInflater.inflate(
						R.layout.list_item_handle_right, null);
				holder.codeView = (EditText) convertView
						.findViewById(R.id.text);
				convertView.setTag(holder);
			} else {
				holder = (ViewHolder) convertView.getTag();
			}

			// Fill EditText with the value you have in data source
			holder.codeView.setText(backupList.get(position));
			holder.codeView.setId(position);

			// we need to update adapter once we finish with editing
			holder.codeView
					.setOnFocusChangeListener(new OnFocusChangeListener() {
						@Override
						public void onFocusChange(View v, boolean hasFocus) {
							if (!hasFocus) {
								final int position = v.getId();
								final EditText Caption = (EditText) v;
								backupList.set(position, Caption.getText()
										.toString());
							}
						}
					});

			return convertView;
		}
	}

}
