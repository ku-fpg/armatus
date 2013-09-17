package edu.kufpg.armatus.radialmenu;

import java.util.List;

import android.view.MotionEvent;
import android.view.View;

import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuRenderer;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuView;

public class RadialMenuSpanRenderer extends RadialMenuRenderer {
	private View mParentView;
	private RadialMenuView mRadialMenuView;

	public RadialMenuSpanRenderer(View view, boolean alt,
			float mThick, float radius) {
		super(null, alt, mThick, radius);
		mParentView = view;
		mRadialMenuView = renderView();
	}

	@Override
	public RadialMenuView renderView() {
		mRadialMenuView = new RadialMenuView(mParentView.getContext(), this);
		return mRadialMenuView;
	}

	@Override
	public void setRadialMenuContent(List<RadialMenuItem> radialMenuContent) {
		super.setRadialMenuContent(radialMenuContent);
		mRadialMenuView.setMenuContent(radialMenuContent);
	}
	
	public boolean touchMenuView(MotionEvent event) {
		return mRadialMenuView.gestureHandler(event);
	}

}
