package edu.kufpg.armatus.radialmenu;

import java.util.List;

import com.google.common.collect.Lists;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem.OnClickListener;

import edu.kufpg.armatus.R;

import android.app.Activity;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.ImageSpan;
import android.widget.RelativeLayout;
import android.widget.TextView;
import android.widget.Toast;

public class RadialMenuActivity extends Activity {
	private RadialMenuSpanRenderer mRenderer;
	private RelativeLayout mHolderLayout;
	private TextView mTextView;
	public RadialMenuItem mMenuContactItem, mMenuMainItem, mMenuAboutItem,
	mMenuTest0Item, mMenuTest1Item, mMenuTest2Item, mMenuTest3Item, mMenuTest4Item;
	private List<List<RadialMenuItem>> mMenuItemLists = Lists.newArrayList();
	private List<RadialMenuItem> mAllItems = Lists.newArrayList();
	private List<RadialMenuItem> mMenuItems0, mMenuItems1, mMenuItems2, mMenuItems3, mMenuItems4;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.radial_menu_activity);

		mHolderLayout = (RelativeLayout) findViewById(R.id.radial_menu_layout);
		mTextView = (TextView) findViewById(R.id.glyph_span_text_view2);
		
		mRenderer = new RadialMenuSpanRenderer(mHolderLayout, true, 40, 180);		
		mMenuContactItem = new RadialMenuItem(getResources().getString(R.string.contact),getResources().getString(R.string.contact));
		mMenuMainItem = new RadialMenuItem(getResources().getString(R.string.main_menu), getResources().getString(R.string.main_menu));
		mMenuAboutItem = new RadialMenuItem(getResources().getString(R.string.about), getResources().getString(R.string.about));
		mMenuTest0Item = new RadialMenuItem(getResources().getString(R.string.test0), getResources().getString(R.string.test0));
		mMenuTest1Item = new RadialMenuItem(getResources().getString(R.string.test1), getResources().getString(R.string.test1));
		mMenuTest2Item = new RadialMenuItem(getResources().getString(R.string.test2), getResources().getString(R.string.test2));
		mMenuTest3Item = new RadialMenuItem(getResources().getString(R.string.test3), getResources().getString(R.string.test3));
		mMenuTest4Item = new RadialMenuItem(getResources().getString(R.string.test4), getResources().getString(R.string.test4));
		
		mAllItems.add(mMenuTest0Item);
		mAllItems.add(mMenuTest1Item);
		mAllItems.add(mMenuTest2Item);
		mAllItems.add(mMenuTest3Item);
		mAllItems.add(mMenuTest4Item);
		mAllItems.add(mMenuContactItem);
		mAllItems.add(mMenuMainItem);
		mAllItems.add(mMenuAboutItem);
		
		mMenuItemLists.add(mMenuItems0);
		mMenuItemLists.add(mMenuItems1);
		mMenuItemLists.add(mMenuItems2);
		mMenuItemLists.add(mMenuItems3);
		mMenuItemLists.add(mMenuItems4);
		for (int i = 0; i < mMenuItemLists.size(); i++) {
			mMenuItemLists.set(i, Lists.newArrayList(mMenuContactItem, mMenuMainItem, mMenuAboutItem));
			mMenuItemLists.get(i).add(mAllItems.get(i));
		}
		mHolderLayout.addView(mRenderer.renderView());

		OnClickListener listener = new OnClickListener() {
			@Override
			public void onClick(String menuId, String menuName) {
				Toast.makeText(RadialMenuActivity.this, menuId, Toast.LENGTH_SHORT).show();
			}
		};
		for (RadialMenuItem item : mAllItems) {
			item.setOnClickListener(listener);
		}
		
		for (int i = 0; i < mMenuItemLists.size(); i++) {
			mTextView.append(" ");
		}
		Spannable textSpans = new SpannableString(mTextView.getText());
		for (int i = 0; i < mMenuItemLists.size(); i++) {
			Drawable d = getResources().getDrawable(R.drawable.ic_launcher);
			d.setBounds(0, 0, d.getIntrinsicWidth(), d.getIntrinsicHeight());
			ImageSpan imageSpan = new ImageSpan(d, ImageSpan.ALIGN_BASELINE);
			textSpans.setSpan(imageSpan, i, i + 1, Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
			RadialMenuSpan menuSpan = new RadialMenuSpan(mMenuItemLists.get(i));
			textSpans.setSpan(menuSpan, i, i + 1, Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
		}
		mTextView.setText(textSpans);
		mTextView.setMovementMethod(RadialMenuMovementMethod.getInstance(mRenderer));
	}
}
