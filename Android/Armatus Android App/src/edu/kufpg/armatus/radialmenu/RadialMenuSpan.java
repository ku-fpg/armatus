package edu.kufpg.armatus.radialmenu;

import java.util.List;

import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem;

import android.text.style.ClickableSpan;
import android.view.View;

public class RadialMenuSpan extends ClickableSpan {
	
	private List<RadialMenuItem> mMenuContent;
	
	public RadialMenuSpan(List<RadialMenuItem> menuContent) {
		mMenuContent = menuContent;
	}
	
	@Override
	public void onClick(View widget) {}
	
	public List<RadialMenuItem> getMenuContent() {
		return mMenuContent;
	}
	
	public void setMenuContent(List<RadialMenuItem> menuContent) {
		mMenuContent = menuContent;
	}

}
