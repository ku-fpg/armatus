package com.espian.showcaseview;

import com.espian.showcaseview.targets.PointTarget;
import com.espian.showcaseview.targets.ViewTarget;

import android.app.Activity;
import android.view.View;

public class ShowcaseViewBuilder {

    private final ShowcaseView mShowcaseView;

    public ShowcaseViewBuilder(Activity activity) {
        mShowcaseView = new ShowcaseView(activity);
    }

    public ShowcaseViewBuilder(ShowcaseView showcaseView) {
        mShowcaseView = showcaseView;
    }

    public ShowcaseViewBuilder(Activity activity, int showcaseLayoutViewId) {
        mShowcaseView = (ShowcaseView) activity.getLayoutInflater().inflate(showcaseLayoutViewId, null);
    }

    public ShowcaseViewBuilder setShowcaseNoView() {
    	mShowcaseView.setShowcase(ShowcaseView.NONE);
        //showcaseView.setShowcaseNoView();
        return this;
    }

    public ShowcaseViewBuilder setShowcaseView(View view) {
    	mShowcaseView.setShowcase(new ViewTarget(view));
        //showcaseView.setShowcaseView(view);
        return this;
    }

    public ShowcaseViewBuilder setShowcasePosition(int x, int y) {
    	mShowcaseView.setShowcase(new PointTarget(x, y));
        //showcaseView.setShowcasePosition(x, y);
        return this;
    }

    public ShowcaseViewBuilder setShowcaseItem(int itemType, int actionItemId, Activity activity) {
        mShowcaseView.setShowcaseItem(itemType, actionItemId, activity);
        return this;
    }

    public ShowcaseViewBuilder setShowcaseIndicatorScale(float scale) {
    	mShowcaseView.setScaleMultiplier(scale);
        //showcaseView.setShowcaseIndicatorScale(scale);
        return this;
    }

    public ShowcaseViewBuilder overrideButtonClick(View.OnClickListener listener) {
        mShowcaseView.setOnButtonClickListener(listener);
        return this;
    }

    public ShowcaseViewBuilder animateGesture(float offsetStartX, float offsetStartY, float offsetEndX, float offsetEndY) {
        mShowcaseView.animateGesture(offsetStartX, offsetStartY, offsetEndX, offsetEndY);
        return this;
    }

//    public ShowcaseViewBuilder setTextColors(int titleTextColor, int detailTextColor) {
//        showcaseView.setTextColors(titleTextColor, detailTextColor);
//        return this;
//    }

    public ShowcaseViewBuilder setText(String titleText, String subText) {
        mShowcaseView.setText(titleText, subText);
        return this;
    }

    public ShowcaseViewBuilder setText(int titleText, int subText) {
        mShowcaseView.setText(titleText, subText);
        return this;
    }

    public ShowcaseViewBuilder pointTo(View view) {
    	mShowcaseView.pointTo(new ViewTarget(view));
        //showcaseView.pointTo(view);
        return this;
    }

    public ShowcaseViewBuilder pointTo(float x, float y) {
    	mShowcaseView.pointTo(new PointTarget((int) x, (int) y));
        //showcaseView.pointTo(x, y);
        return this;
    }

    public ShowcaseViewBuilder setConfigOptions(ShowcaseView.ConfigOptions configOptions) {
        mShowcaseView.setConfigOptions(configOptions);
        return this;
    }

    public ShowcaseView build(){
        return mShowcaseView;
    }
}