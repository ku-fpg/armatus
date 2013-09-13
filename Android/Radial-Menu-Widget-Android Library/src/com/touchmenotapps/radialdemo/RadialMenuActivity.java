package com.touchmenotapps.radialdemo;

import java.util.ArrayList;

import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuRenderer;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuRenderer.OnRadailMenuClick;

import android.os.Bundle;
import android.support.v4.app.FragmentActivity;
import android.support.v4.app.FragmentManager;
import android.widget.FrameLayout;

/**
 * 
 * @author Arindam Nath
 *
 */
public class RadialMenuActivity extends FragmentActivity {

	//Variable declarations
	private RadialMenuRenderer mRenderer;
	private FrameLayout mHolderLayout;
	public RadialMenuItem menuContactItem, menuMainItem, menuAboutItem, menuTest1Item, menuTest2Item, menuTest3Item, menuTest4Item, menuTest5Item, menuTest6Item, menuTest7Item;
	private ArrayList<RadialMenuItem> mMenuItems = new ArrayList<RadialMenuItem>(0);

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.layout_holder);
		
		//Init the frame layout
		mHolderLayout = (FrameLayout) findViewById(R.id.fragment_container);
		// Init the Radial Menu and menu items
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
		//Handle the menu item interactions
		menuContactItem.setOnRadialMenuClickListener(new OnRadailMenuClick() {
			@Override
			public void onRadailMenuClickedListener(String id) {
				//Can edit based on preference. Also can add animations here.
				getSupportFragmentManager().popBackStack(null, FragmentManager.POP_BACK_STACK_INCLUSIVE);
				getSupportFragmentManager().beginTransaction().replace(mHolderLayout.getId(), new RadialMenuContactFragment()).commit();
			}
		});
		
		menuMainItem.setOnRadialMenuClickListener(new OnRadailMenuClick() {
			@Override
			public void onRadailMenuClickedListener(String id) {
				//Can edit based on preference. Also can add animations here.
				getSupportFragmentManager().popBackStack(null, FragmentManager.POP_BACK_STACK_INCLUSIVE);
				getSupportFragmentManager().beginTransaction().replace(mHolderLayout.getId(), new RadialMenuMainFragment()).commit();
			}
		});
		
		menuAboutItem.setOnRadialMenuClickListener(new OnRadailMenuClick() {
			@Override
			public void onRadailMenuClickedListener(String id) {
				//Can edit based on preference. Also can add animations here.
				getSupportFragmentManager().popBackStack(null, FragmentManager.POP_BACK_STACK_INCLUSIVE);
				getSupportFragmentManager().beginTransaction().replace(mHolderLayout.getId(), new RadialMenuAboutFragment()).commit();
			}
		});
	}
	
	@Override
	protected void onResume() {
		super.onResume();
		//Init with home fragment
		getSupportFragmentManager().popBackStack(null, FragmentManager.POP_BACK_STACK_INCLUSIVE);
		getSupportFragmentManager().beginTransaction().replace(mHolderLayout.getId(), new RadialMenuMainFragment()).commit();
	}
}
