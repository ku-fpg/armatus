package com.kufpg.armatus.drag;

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
import java.util.Random;

import com.kufpg.armatus.R;
import com.kufpg.armatus.console.ConsoleActivity;

import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import android.graphics.Typeface;
import android.util.Log;
import android.view.View;
import android.view.View.OnLongClickListener;
import android.view.ViewGroup.LayoutParams;
import android.widget.ImageView;
import android.widget.LinearLayout;

import ca.laplanete.mobile.pageddragdropgrid.Item;
import ca.laplanete.mobile.pageddragdropgrid.Page;
import ca.laplanete.mobile.pageddragdropgrid.PagedDragDropGrid;
import ca.laplanete.mobile.pageddragdropgrid.PagedDragDropGridAdapter;

public class KeywordSwapGridAdapter implements PagedDragDropGridAdapter {

	private Context mContext;
	private PagedDragDropGrid mGridview;
	private Typeface mTypeface;
	private Random mRandom = new Random();
	private final int CHAR_WIDTH_PIXELS = 15;
	private final int DEFAULT_MAX_CHARS = 10;

	List<Page> mPages = new ArrayList<Page>();

	public KeywordSwapGridAdapter(Context context, PagedDragDropGrid gridView, String entryContents) {
		super();
		mContext = context;
		mGridview = gridView;
		mTypeface = Typeface.createFromAsset(context.getAssets(), ConsoleActivity.TYPEFACE);

		Page page1 = new Page();
		List<Item> items = new ArrayList<Item>();
		String[] sentence = entryContents.split(ConsoleActivity.WHITESPACE);
		for (int i = 0; i < sentence.length; i++) {
			items.add(new Item(i, getThumb(sentence[i])));
		}
		page1.setItems(items);
		mPages.add(page1);
	}

	@Override
	public int pageCount() {
		return mPages.size();
	}

	private List<Item> itemsInPage(int page) {
		if (mPages.size() > page) {
			return mPages.get(page).getItems();
		}	
		return Collections.emptyList();
	}

	@Override
	public View view(int page, int index) {

		LinearLayout layout = new LinearLayout(mContext);
		layout.setOrientation(LinearLayout.VERTICAL);

		ImageView icon = new ImageView(mContext);
		Item item = getItem(page, index);
		icon.setImageBitmap(item.getDrawable());
		icon.setPadding(15, 15, 15, 15);

		layout.addView(icon);

		//		//TextView label = new TextView(context);
		//		label.setText(item.getName());	
		//		label.setTextColor(Color.BLACK);
		//		label.setGravity(Gravity.TOP | Gravity.CENTER_HORIZONTAL);

		//label.setLayoutParams(new LayoutParams(LayoutParams.MATCH_PARENT,LayoutParams.WRAP_CONTENT));

		layout.setLayoutParams(new LinearLayout.LayoutParams(LayoutParams.WRAP_CONTENT, LayoutParams.WRAP_CONTENT));

		layout.setBackground(mContext.getResources().getDrawable(R.drawable.list_selector_holo_light));
		layout.setClickable(true);
		layout.setOnLongClickListener(new OnLongClickListener() {
			@Override
			public boolean onLongClick(View v) {
				return mGridview.onLongClick(v);
			}
		});

		//layout.addView(label);
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
		for (Page page : mPages) {
			Log.d("Page", Integer.toString(i++));

			for (Item item : page.getItems()) {
				Log.d("Item", Long.toString(item.getId()));
			}
		}
	}

	private Page getPage(int pageIndex) {
		return mPages.get(pageIndex);
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

	private Bitmap getThumb(String s) {
		int x = 150;
		if (s.length() <= DEFAULT_MAX_CHARS) {
			x = DEFAULT_MAX_CHARS * CHAR_WIDTH_PIXELS;
		} else {
			x = s.length() * CHAR_WIDTH_PIXELS;
		}
		int y = 100;

		Bitmap bmp = Bitmap.createBitmap(x, y, Bitmap.Config.RGB_565);
		Canvas canvas = new Canvas(bmp);
		Paint paint = new Paint();

		paint.setColor(Color.rgb(mRandom.nextInt(128), mRandom.nextInt(128), mRandom.nextInt(128)));
		paint.setTextSize(24);
		paint.setTypeface(mTypeface);
		paint.setFlags(Paint.ANTI_ALIAS_FLAG);
		canvas.drawRect(new Rect(0, 0, x, y), paint);
		paint.setColor(Color.WHITE);
		paint.setTextAlign(Paint.Align.CENTER);
		canvas.drawText(s, x/2.0f, y/2.0f, paint);

		return bmp;
	}

}
