package com.kufpg.androidhermit.drag;

/**
 * Copyright 2012 
 * 
 * Nicolas Desjardins  
 * https://github.com/mrKlar
 * 
 * Facilite solutions
 * http://www.facilitesolutions.com/
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 * 
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.console.ConsoleActivity;

import android.content.Context;
import android.graphics.Color;
import android.util.Log;
import android.view.Gravity;
import android.view.View;
import android.view.View.OnLongClickListener;
import android.view.ViewGroup.LayoutParams;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import ca.laplanete.mobile.pageddragdropgrid.Item;
import ca.laplanete.mobile.pageddragdropgrid.Page;
import ca.laplanete.mobile.pageddragdropgrid.PagedDragDropGrid;
import ca.laplanete.mobile.pageddragdropgrid.PagedDragDropGridAdapter;

public class KeywordSwapGridAdapter implements PagedDragDropGridAdapter {

	private Context context;
	private PagedDragDropGrid gridview;

	List<Page> pages = new ArrayList<Page>();

	public KeywordSwapGridAdapter(Context context, PagedDragDropGrid gridView, String entryContents) {
		super();
		this.context = context;
		this.gridview = gridView;

		Page page1 = new Page();
		List<Item> items = new ArrayList<Item>();
		String[] sentence = entryContents.split(ConsoleActivity.WHITESPACE);
		for (int i = 0; i < sentence.length; i++) {
			items.add(new Item(i, sentence[i], R.drawable.ks_hermit));
		}
		page1.setItems(items);
		pages.add(page1);
	}

	@Override
	public int pageCount() {
		return pages.size();
	}

	private List<Item> itemsInPage(int page) {
		if (pages.size() > page) {
			return pages.get(page).getItems();
		}	
		return Collections.emptyList();
	}

	@Override
	public View view(int page, int index) {

		LinearLayout layout = new LinearLayout(context);
		layout.setOrientation(LinearLayout.VERTICAL);

		ImageView icon = new ImageView(context);
		Item item = getItem(page, index);
		icon.setImageResource(item.getDrawable());
		icon.setPadding(15, 15, 15, 15);

		layout.addView(icon);

		TextView label = new TextView(context);
		label.setText(item.getName());	
		label.setTextColor(Color.BLACK);
		label.setGravity(Gravity.TOP | Gravity.CENTER_HORIZONTAL);

		label.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT,LayoutParams.WRAP_CONTENT));

		layout.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT));

		layout.setBackground(context.getResources().getDrawable(R.drawable.list_selector_holo_light));
		layout.setClickable(true);
		layout.setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				return gridview.onLongClick(v);
			}
		});

		layout.addView(label);
		return layout;
	}

	private Item getItem(int page, int index) {
		List<Item> items = itemsInPage(page);
		return items.get(index);
	}

	@Override
	public int rowCount() {
		return AUTOMATIC;
	}

	@Override
	public int columnCount() {
		return AUTOMATIC;
	}

	@Override
	public int itemCountInPage(int page) {
		return itemsInPage(page).size();
	}

	public void printLayout() {
		int i=0;
		for (Page page : pages) {
			Log.d("Page", Integer.toString(i++));

			for (Item item : page.getItems()) {
				Log.d("Item", Long.toString(item.getId()));
			}
		}
	}

	private Page getPage(int pageIndex) {
		return pages.get(pageIndex);
	}

	@Override
	public void swapItems(int pageIndex, int itemIndexA, int itemIndexB) {
		getPage(pageIndex).swapItems(itemIndexA, itemIndexB);
	}

	@Override
	public void moveItemToPreviousPage(int pageIndex, int itemIndex) {
		int leftPageIndex = pageIndex-1;
		if (leftPageIndex >= 0) {
			Page startpage = getPage(pageIndex);
			Page landingPage = getPage(leftPageIndex);

			Item item = startpage.removeItem(itemIndex);
			landingPage.addItem(item);	
		}	
	}

	@Override
	public void moveItemToNextPage(int pageIndex, int itemIndex) {
		int rightPageIndex = pageIndex+1;
		if (rightPageIndex < pageCount()) {
			Page startpage = getPage(pageIndex);
			Page landingPage = getPage(rightPageIndex);

			Item item = startpage.removeItem(itemIndex);
			landingPage.addItem(item);			
		}	
	}

	@Override
	public void deleteItem(int pageIndex, int itemIndex) {
		getPage(pageIndex).deleteItem(itemIndex);
	}

}
