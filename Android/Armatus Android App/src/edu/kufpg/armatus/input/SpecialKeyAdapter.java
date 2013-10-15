package edu.kufpg.armatus.input;

import java.util.List;

import edu.kufpg.armatus.R;

import android.content.Context;
import android.graphics.Typeface;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ArrayAdapter;
import android.widget.TextView;

public class SpecialKeyAdapter extends ArrayAdapter<String> {

	private Context mContext;
	private Typeface mTypeface;
	
	public SpecialKeyAdapter(Context context, List<String> objects) {
		super(context, R.layout.special_key_entry, objects);
		mContext = context;
	}

	@Override
	public View getView(int position, View convertView, ViewGroup parent) {
		SpecialKeyHolder holder;
		if (convertView == null) {
			LayoutInflater inflater = LayoutInflater.from(mContext);
			convertView = inflater.inflate(R.layout.special_key_entry, parent, false);
			holder = new SpecialKeyHolder();
			holder.key = (TextView) convertView.findViewById(R.id.special_key_button);
			convertView.setTag(holder);
		} else {
			holder = (SpecialKeyHolder) convertView.getTag();
		}
		
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
		TextView key;
	}
}
