/*
 * Copyright (C) 2010 Eric Harlow
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.ericharlow.dragndrop;

import java.util.List;

import com.ericharlow.dragndrop.R;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;

public final class DragNDropAdapter extends BaseAdapter implements DropListener {
	
    private LayoutInflater mInflater;
    private List<String> mContent;

    public DragNDropAdapter(Context context, List<String> content) {
        mInflater = LayoutInflater.from(context);
    	mContent = content;
    }
    
    /**
     * The number of items in the list
     * @see android.widget.ListAdapter#getCount()
     */
    public int getCount() {
        return mContent.size();
    }

    /**
     * Since the data comes from an array, just returning the index is
     * sufficient to get at the data. If we were using a more complex data
     * structure, we would return whatever object represents one row in the
     * list.
     *
     * @see android.widget.ListAdapter#getItem(int)
     */
    public String getItem(int position) {
        return mContent.get(position);
    }

    /**
     * Use the array index as a unique id.
     * @see android.widget.ListAdapter#getItemId(int)
     */
    public long getItemId(int position) {
        return position;
    }

    /**
     * Make a view to hold each row.
     *
     * @see android.widget.ListAdapter#getView(int, android.view.View,
     *      android.view.ViewGroup)
     */
    public View getView(final int position, View convertView, ViewGroup parent) {
        // A ViewHolder keeps references to children views to avoid unneccessary calls
        // to findViewById() on each row.
        ViewHolder holder;

        // When convertView is not null, we can reuse it directly, there is no need
        // to reinflate it. We only inflate a new View when the convertView supplied
        // by ListView is null.
        if (convertView == null) {
            convertView = mInflater.inflate(R.layout.drag_item, null);

            // Creates a ViewHolder and store references to the two children views
            // we want to bind data to.
            holder = new ViewHolder();
            holder.dragIndicator = (ImageView) convertView.findViewById(R.id.drag_indicator);
            holder.text = (TextView) convertView.findViewById(R.id.drag_item_text);

            convertView.setTag(holder);
        } else {
            // Get the ViewHolder back to get fast access to the TextView
            // and the ImageView.
            holder = (ViewHolder) convertView.getTag();
        }
        
        if (canBeDragged(position)) {
        	holder.dragIndicator.setVisibility(View.VISIBLE);
        } else {
        	holder.dragIndicator.setVisibility(View.INVISIBLE);
        }
        
        // Bind the data efficiently with the holder.
        holder.text.setText(mContent.get(position));

        return convertView;
    }

    static class ViewHolder {
    	ImageView dragIndicator;
        TextView text;
    }

	public void onRemove(int which) {
		if (which < 0 || which > mContent.size()) return;		
		mContent.remove(which);
	}

	public void onDrop(int from, int to) {
		String fromMessage = mContent.get(from);
		String toMessage = mContent.get(to);
		mContent.set(from, toMessage);
		mContent.set(to, fromMessage);
	}
	
	public boolean canBeDragged(int index) {
		if (index == 0 || index == mContent.size() - 1) {
			return true;
		}
		return false;
	}
}