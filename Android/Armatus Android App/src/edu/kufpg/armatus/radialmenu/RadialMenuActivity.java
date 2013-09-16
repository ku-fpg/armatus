package edu.kufpg.armatus.radialmenu;

import java.util.ArrayList;

import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem.OnClickListener;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuRenderer;

import edu.kufpg.armatus.R;

import android.app.Activity;
import android.os.Bundle;
import android.widget.RelativeLayout;
import android.widget.Toast;

public class RadialMenuActivity extends Activity {

	//Variable declarations
	private RadialMenuRenderer mRenderer;
	private RelativeLayout mHolderLayout;
	public RadialMenuItem menuContactItem, menuMainItem, menuAboutItem, menuTest1Item, menuTest2Item, menuTest3Item, menuTest4Item, menuTest5Item, menuTest6Item, menuTest7Item;
	private ArrayList<RadialMenuItem> mMenuItems = new ArrayList<RadialMenuItem>(0);

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.radial_menu_activity);
		
		// Init the Radial Menu and menu items
		mHolderLayout = (RelativeLayout) findViewById(R.id.radial_menu_layout);
		mRenderer = new RadialMenuRenderer(mHolderLayout, true, 40, 180);		
		menuContactItem = new RadialMenuItem(getResources().getString(R.string.contact),getResources().getString(R.string.contact));
		menuMainItem = new RadialMenuItem(getResources().getString(R.string.main_menu), getResources().getString(R.string.main_menu));
		menuAboutItem = new RadialMenuItem(getResources().getString(R.string.about), getResources().getString(R.string.about));
		menuTest1Item = new RadialMenuItem(getResources().getString(R.string.test1), getResources().getString(R.string.test1));
		menuTest2Item = new RadialMenuItem(getResources().getString(R.string.test2), getResources().getString(R.string.test2));
		menuTest3Item = new RadialMenuItem(getResources().getString(R.string.test3), getResources().getString(R.string.test3));
		menuTest4Item = new RadialMenuItem(getResources().getString(R.string.test4), getResources().getString(R.string.test4));
		menuTest5Item = new RadialMenuItem(getResources().getString(R.string.test5), getResources().getString(R.string.test5));
		menuTest6Item = new RadialMenuItem(getResources().getString(R.string.test6), getResources().getString(R.string.test6));
		menuTest7Item = new RadialMenuItem(getResources().getString(R.string.test7), getResources().getString(R.string.test7));
		//Add the menu Items
		mMenuItems.add(menuMainItem);
		mMenuItems.add(menuAboutItem);
		mMenuItems.add(menuContactItem);
		mMenuItems.add(menuTest1Item);
		mMenuItems.add(menuTest2Item);
		mMenuItems.add(menuTest3Item);
		mMenuItems.add(menuTest4Item);
		mMenuItems.add(menuTest5Item);
		mMenuItems.add(menuTest6Item);
		mMenuItems.add(menuTest7Item);
		mRenderer.setRadialMenuContent(mMenuItems);
		mHolderLayout.addView(mRenderer.renderView());
		
		OnClickListener listener = new OnClickListener() {
			@Override
			public void onClick(String id) {
				Toast.makeText(RadialMenuActivity.this, id, Toast.LENGTH_SHORT).show();
			}
		};
		
		for (RadialMenuItem item : mMenuItems) {
			item.setOnClickListener(listener);
		}
	}
}
