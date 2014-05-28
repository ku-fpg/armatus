package edu.kufpg.armatus.input;

import java.util.List;

import android.app.Activity;
import android.graphics.Typeface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.EditText;
import edu.kufpg.armatus.R;

public class SpecialKeyAdapter extends ArrayAdapter<String> {

	private Activity mActivity;
	private Typeface mTypeface;
	
	public SpecialKeyAdapter(Activity activity, List<String> objects) {
		super(activity, R.layout.special_key_entry, objects);
		mActivity = activity;
	}

	@Override
	public View getView(final int position, View convertView, ViewGroup parent) {
		SpecialKeyHolder holder;
		if (convertView == null) {
			LayoutInflater inflater = LayoutInflater.from(mActivity);
			convertView = inflater.inflate(R.layout.special_key_entry, parent, false);
			holder = new SpecialKeyHolder();
			holder.key = (Button) convertView.findViewById(R.id.special_key_button);
			convertView.setTag(holder);
		} else {
			holder = (SpecialKeyHolder) convertView.getTag();
		}
		
		OnClickListener listener = (OnClickListener) holder.key.getTag();
		if (listener == null) {
			listener = new OnClickListener() {
				@Override
				public void onClick(View v) {
					if (mActivity.getCurrentFocus() instanceof EditText) {
						EditText et = (EditText) mActivity.getCurrentFocus();
						et.getText().insert(et.getSelectionStart(), getItem(position));
					}
				}
			};
			holder.key.setTag(listener);
		}
		holder.key.setOnClickListener(listener);
		holder.key.setText(getItem(position));
		if (mTypeface != null) {
			holder.key.setTypeface(mTypeface);
		}
		
		return convertView;
	}
	
	public void setTypeface(Typeface tf) {
		mTypeface = tf;
	}

	private static class SpecialKeyHolder {
		Button key;
	}
}
