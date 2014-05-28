package edu.kufpg.armatus.console.tutorial;

import android.os.Bundle;

import com.espian.showcaseview.OnShowcaseEventListener;
import com.espian.showcaseview.ShowcaseView;
import com.espian.showcaseview.ShowcaseView.ConfigOptions;
import com.espian.showcaseview.ShowcaseViews;
import com.espian.showcaseview.ShowcaseViews.ItemViewProperties;
import com.espian.showcaseview.targets.Target;
import com.espian.showcaseview.targets.ViewTarget;
import com.jeremyfeinstein.slidingmenu.lib.SlidingMenu.OnOpenedListener;

import edu.kufpg.armatus.console.ConsoleActivity;

public class TutorialConsoleActivity extends ConsoleActivity {

	private ShowcaseView mShowcaseView;
	private ShowcaseViews mShowcaseViews;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		setSoftKeyboardVisibility(false);
		final Target target = new ViewTarget(mConsoleListLayout);
		final ConfigOptions co = new ConfigOptions();
		mShowcaseView = ShowcaseView.insertShowcaseView(target, this,
				"Access other commands", "Swipe to the right to view the full list of commands", co);
		mShowcaseView.setOnShowcaseEventListener(new OnShowcaseEventListener() {
			@Override
			public void onShowcaseViewHide(ShowcaseView showcaseView) {}

			@Override
			public void onShowcaseViewDidHide(ShowcaseView showcaseView) {
				//				mShowcaseView.post(new Runnable() {
				//					@Override
				//					public void run() {
				setupCommandExpandableMenuTutorial();
				//					}
				//				});
			}

			@Override
			public void onShowcaseViewShow(ShowcaseView showcaseView) {
				mShowcaseView.post(new Runnable() {
					@Override
					public void run() {
						final int targetX = target.getPoint().x;
						final int targetY = target.getPoint().y;
						mShowcaseView.animateGesture(targetX, targetY, targetX - 200, targetY);
					}
				});
				mSlidingMenu.setOnOpenedListener(new OnOpenedListener() {
					@Override
					public void onOpened() {
						if (mSlidingMenu.isSecondaryMenuShowing()) {
							mShowcaseView.hide();
							mSlidingMenu.setOnOpenedListener(null);
							//							setupCommandExpandableMenuTutorial();
						}
					}
				});
			}
		});
		mShowcaseView.show();
	}

	private void setupCommandExpandableMenuTutorial() {
		final String title = "Command expandable menu";

		mShowcaseViews = new ShowcaseViews(this);
		mShowcaseViews.addView(new ItemViewProperties(mCommandExpandableSearchView,	title, "Filter the commands using this."));
		mShowcaseViews.addView(new ItemViewProperties(mCommandExpandableMenuView, title, "Click on a group to expand all of the commands under that tag."));
		mShowcaseViews.show();
	}

}
