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

import java.util.ArrayList;

import com.ericharlow.dragndrop.R;

import android.app.ListActivity;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ListAdapter;
import android.widget.ListView;
import android.widget.TextView;

public class DragNDropListActivity extends ListActivity {

	private static String[] mListContent = { "Item 1", "Item 2", "Item 3", "Item 4", "Item 5", "Item 6", "Item 7" };

	/** Called when the activity is first created. */
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setContentView(R.layout.dragndrop_listview);

		ArrayList<String> content = new ArrayList<String>(mListContent.length);
		for (int i=0; i < mListContent.length; i++) {
			content.add(mListContent[i]);
		}

		setListAdapter(new DragNDropAdapter(this, content));
		ListView listView = getListView();

		if (listView instanceof DragNDropListView) {
			((DragNDropListView) listView).setDropListener(mDropListener);
			((DragNDropListView) listView).setDragListener(mDragListener);
		}

		Button button = (Button) findViewById(R.id.button);
		button.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				for (int i = 0; i < getListView().getChildCount(); i++) {
					Log.d("TESTTEST", ((TextView) ((LinearLayout) getListView().
							getChildAt(i)).getChildAt(1)).getText().toString());
				}
			}
		});
	}

	private DropListener mDropListener = 
			new DropListener() {
		public void onDrop(int from, int to) {
			ListAdapter adapter = getListAdapter();
			if (adapter instanceof DragNDropAdapter && canBeSwapped(from, to)) {
				((DragNDropAdapter)adapter).onDrop(from, to);
				getListView().getChildAt(to).setDrawingCacheEnabled(false);
				getListView().getChildAt(from).setDrawingCacheEnabled(false);
				getListView().invalidateViews();
			}
		}
	};

	private DragListener mDragListener =
			new DragListener() {

		View mHighlightedView = null;
		int dragViewColor = 0xe0103010; //Dark green
		int highlightColor = 0xffd17d10; //Light orange
		int highlightedIndex = -1;
		int defaultBackgroundColor;

		@Override
		public synchronized void onDrag(int draggedOverIndex, View dragView, ListView listView) {
			if (highlightedIndex != draggedOverIndex) {
				mHighlightedView = null;
				if (highlightedIndex != -1) {
					mHighlightedView = listView.getChildAt(highlightedIndex);
					if (mHighlightedView != null) {
						mHighlightedView.setBackgroundColor(defaultBackgroundColor);
					}
				}

				mHighlightedView = listView.getChildAt(draggedOverIndex);
				int draggingIndex = getListView().indexOfChild(dragView);
				if (mHighlightedView != null && canBeSwapped(draggingIndex, draggedOverIndex)) {
					mHighlightedView.setBackgroundColor(highlightColor);
				}
				highlightedIndex = draggedOverIndex;
			}
		}

		@Override
		public void onStartDrag(View dragView) {
			dragView.setVisibility(View.INVISIBLE);
			defaultBackgroundColor = dragView.getDrawingCacheBackgroundColor();
			dragView.setBackgroundColor(dragViewColor);
			dragView.setAlpha(0.5f);
			ImageView iv = (ImageView)dragView.findViewById(R.id.drag_indicator);
			if (iv != null) iv.setVisibility(View.INVISIBLE);
		}

		@Override
		public void onStopDrag(View dragView) {
			if (mHighlightedView != null) {
				mHighlightedView.setBackgroundColor(defaultBackgroundColor);
			}

			dragView.setVisibility(View.VISIBLE);
			dragView.setBackgroundColor(defaultBackgroundColor);
			dragView.setAlpha(1);
			ImageView iv = (ImageView)dragView.findViewById(R.id.drag_indicator);
			if (iv != null) iv.setVisibility(View.VISIBLE);
		}

	};

	private boolean canBeSwapped(int pos1, int pos2) {
		int childCount = getListView().getAdapter().getCount();
		if ((pos1 == 0 && pos2 == childCount - 1) ||
				(pos2 == 0 && pos1 == childCount - 1)) {
			return true;
		} else {
			return false;
		}
	}
}