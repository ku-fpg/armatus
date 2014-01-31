package com.espian.showcaseview;

import android.app.Activity;
import android.content.Context;
import android.os.Handler;
import android.view.View;
import android.view.ViewGroup;

import java.util.ArrayList;
import java.util.List;

public class ShowcaseViews {

	private final List<ShowcaseView> mViews = new ArrayList<ShowcaseView>();
	private final List<float[]> mAnimations = new ArrayList<float[]>();
	private final Activity mActivity;
	private OnShowcaseAcknowledgedListener mShowcaseAcknowledgedListener = new OnShowcaseAcknowledgedListener() {
		@Override
		public void onShowcaseAcknowledged(ShowcaseView showcaseView) {
			//DEFAULT LISTENER - DOESN'T DO ANYTHING!
		}
	};

	private static final int ABSOLUTE_COORDINATES = 0;
	private static final int RELATIVE_COORDINATES = 1;

	public static interface OnShowcaseAcknowledgedListener {
		void onShowcaseAcknowledged(ShowcaseView showcaseView);
	}

	public ShowcaseViews(Activity activity) {
		this.mActivity = activity;
	}

	public ShowcaseViews(Activity activity, OnShowcaseAcknowledgedListener acknowledgedListener) {
		this(activity);
		mShowcaseAcknowledgedListener = acknowledgedListener;
	}

	public ShowcaseViews addView(ItemViewProperties properties) {
		ShowcaseViewBuilder builder = new ShowcaseViewBuilder(mActivity);
		if (properties.mmTitleResId == ItemViewProperties.ID_NOT_GIVEN
				|| properties.mmMessageResId == ItemViewProperties.ID_NOT_GIVEN) {
			builder.setText(properties.mmTitle, properties.mmMessage);
		} else {
			builder.setText(properties.mmTitleResId, properties.mmMessageResId);
		}
		builder.setShowcaseIndicatorScale(properties.mmScale)
		.setConfigOptions(properties.mmConfigOptions);

		if(showcaseActionBar(properties)) {
			builder.setShowcaseItem(properties.mmItemType, properties.mmViewId, mActivity);
		} else if (properties.mmViewId == ItemViewProperties.ID_NO_SHOWCASE) {
			builder.setShowcaseNoView();
		} else if (properties.mmViewId == ItemViewProperties.ID_NOT_GIVEN) {
			builder.setShowcaseView(properties.mmView);
		} else {
			builder.setShowcaseView(mActivity.findViewById(properties.mmViewId));
		}

		ShowcaseView showcaseView = builder.build();
		showcaseView.setOnButtonClickListener(createShowcaseViewDismissListener(showcaseView));
		mViews.add(showcaseView);

		mAnimations.add(null);

		return this;
	}

	/**
	 * Add an animated gesture to the view at position viewIndex.
	 * @param viewIndex     The position of the view the gesture should be added to (beginning with 0 for the view which had been added as the first one)
	 * @param offsetStartX  x-offset of the start position
	 * @param offsetStartY  y-offset of the start position
	 * @param offsetEndX    x-offset of the end position
	 * @param offsetEndY    y-offset of the end position
	 * @see com.espian.showcaseview.ShowcaseView#animateGesture(float, float, float, float)
	 * @see com.espian.showcaseview.ShowcaseViews#addAnimatedGestureToView(int, float, float, float, float, boolean)
	 */
	public void addAnimatedGestureToView(int viewIndex, float offsetStartX, float offsetStartY, float offsetEndX, float offsetEndY) throws IndexOutOfBoundsException {
		addAnimatedGestureToView(viewIndex, offsetStartX, offsetStartY, offsetEndX, offsetEndY, false);
	}

	/**
	 * Add an animated gesture to the view at position viewIndex.
	 * @param viewIndex             The position of the view the gesture should be added to (beginning with 0 for the view which had been added as the first one)
	 * @param startX                x-coordinate or x-offset of the start position
	 * @param startY                y-coordinate or x-offset of the start position
	 * @param endX                  x-coordinate or x-offset of the end position
	 * @param endY                  y-coordinate or x-offset of the end position
	 * @param absoluteCoordinates   If true, this will use absolute coordinates instead of coordinates relative to the center of the showcased view
	 */
	public void addAnimatedGestureToView(int viewIndex, float startX, float startY, float endX, float endY, boolean absoluteCoordinates) throws IndexOutOfBoundsException {
		mAnimations.remove(viewIndex);
		mAnimations.add(viewIndex, new float[]{absoluteCoordinates?ABSOLUTE_COORDINATES:RELATIVE_COORDINATES, startX, startY, endX, endY});
	}

	private static boolean showcaseActionBar(ItemViewProperties properties) {
		return properties.mmItemType > ItemViewProperties.ID_NOT_IN_ACTIONBAR;
	}

	private View.OnClickListener createShowcaseViewDismissListener(final ShowcaseView showcaseView) {
		return new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				showcaseView.onClick(showcaseView); //Needed for TYPE_ONE_SHOT
				int fadeOutTime = showcaseView.getConfigOptions().fadeOutDuration;
				if (fadeOutTime > 0) {
					final Handler handler = new Handler();
					handler.postDelayed(new Runnable() {
						@Override
						public void run() {
							showNextView(showcaseView);
						}
					}, fadeOutTime);
				} else {
					showNextView(showcaseView);
				}
			}
		};
	}

	private void showNextView(ShowcaseView showcaseView) {
		if (mViews.isEmpty()) {
			mShowcaseAcknowledgedListener.onShowcaseAcknowledged(showcaseView);
		} else {
			show();
		}
	}

	public void show() {
		if (mViews.isEmpty()) {
			return;
		}
		final ShowcaseView view = mViews.get(0);

		boolean hasShot = mActivity.getSharedPreferences(ShowcaseView.PREFS_SHOWCASE_INTERNAL, Context.MODE_PRIVATE)
				.getBoolean("hasShot" + view.getConfigOptions().showcaseId, false);
		if (hasShot && view.getConfigOptions().shotType == ShowcaseView.TYPE_ONE_SHOT) {
			// The showcase has already been shot once, so we don't need to do show it again.
			view.setVisibility(View.GONE);
			mViews.remove(0);
			mAnimations.remove(0);
			view.getConfigOptions().fadeOutDuration = 0;
			view.performButtonClick();
			return;
		}

		view.setVisibility(View.INVISIBLE);
		((ViewGroup) mActivity.getWindow().getDecorView()).addView(view);
		view.show();

		float[] animation = mAnimations.get(0);
		if (animation != null) {
			view.animateGesture(animation[1], animation[2], animation[3], animation[4], animation[0] == ABSOLUTE_COORDINATES);
		}

		mViews.remove(0);
		mAnimations.remove(0);
	}

	public boolean hasViews(){
		return !mViews.isEmpty();
	}

	public static class ItemViewProperties {

		public static final int ID_NOT_GIVEN = -42;
		public static final int ID_NO_SHOWCASE = -2202;
		public static final int ID_NOT_IN_ACTIONBAR = -1;
		public static final int ID_SPINNER = 0;
		public static final int ID_TITLE = 1;
		public static final int ID_OVERFLOW = 2;
		private static final float DEFAULT_SCALE = 1f;

		protected final String mmTitle;
		protected final int mmTitleResId;
		protected final String mmMessage;
		protected final int mmMessageResId;
		protected final View mmView;
		protected final int mmViewId;
		protected final int mmItemType;
		protected final float mmScale;
		protected final ShowcaseView.ConfigOptions mmConfigOptions;

		public ItemViewProperties(int titleResId, int messageResId) {
			this(null, ID_NO_SHOWCASE, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(String title, String message) {
			this(null, ID_NO_SHOWCASE, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(int viewId, int titleResId, int messageResId) {
			this(null, viewId, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(View view, int titleResId, int messageResId) {
			this(view, ID_NOT_GIVEN, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(int viewId, String title, String message) {
			this(null, viewId, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(View view, String title, String message) {
			this(view, ID_NOT_GIVEN, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(int viewId, int titleResId, int messageResId, float scale) {
			this(null, viewId, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, scale, null);
		}

		public ItemViewProperties(View view, int titleResId, int messageResId, float scale) {
			this(view, ID_NOT_GIVEN, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, scale, null);
		}

		public ItemViewProperties(int viewId, String title, String message, float scale) {
			this(null, viewId, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, scale, null);
		}

		public ItemViewProperties(View view, String title, String message, float scale) {
			this(view, ID_NOT_GIVEN, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, scale, null);
		}

		public ItemViewProperties(int viewId, int titleResId, int messageResId, int itemType) {
			this(null, viewId, null, titleResId, null, messageResId, itemType, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(View view, int titleResId, int messageResId, int itemType) {
			this(view, ID_NOT_GIVEN, null, titleResId, null, messageResId, itemType, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(int viewId, String title, String message, int itemType) {
			this(null, viewId, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, itemType, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(View view, String title, String message, int itemType) {
			this(view, ID_NOT_GIVEN, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, itemType, DEFAULT_SCALE, null);
		}

		public ItemViewProperties(int viewId, int titleResId, int messageResId, int itemType, float scale) {
			this(null, viewId, null, titleResId, null, messageResId, itemType, scale, null);
		}

		public ItemViewProperties(View view, int titleResId, int messageResId, int itemType, float scale) {
			this(view, ID_NOT_GIVEN, null, titleResId, null, messageResId, itemType, scale, null);
		}

		public ItemViewProperties(int viewId, String title, String message, int itemType, float scale) {
			this(null, viewId, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, itemType, scale, null);
		}

		public ItemViewProperties(View view, String title, String message, int itemType, float scale) {
			this(view, ID_NOT_GIVEN, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, itemType, scale, null);
		}

		public ItemViewProperties(int titleResId, int messageResId, ShowcaseView.ConfigOptions configOptions) {
			this(null, ID_NO_SHOWCASE, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(String title, String message, ShowcaseView.ConfigOptions configOptions) {
			this(null, ID_NO_SHOWCASE, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(int viewId, int titleResId, int messageResId, ShowcaseView.ConfigOptions configOptions) {
			this(null, viewId, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(View view, int titleResId, int messageResId, ShowcaseView.ConfigOptions configOptions) {
			this(view, ID_NOT_GIVEN, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(int viewId, String title, String message, ShowcaseView.ConfigOptions configOptions) {
			this(null, viewId, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(View view, String title, String message, ShowcaseView.ConfigOptions configOptions) {
			this(view, ID_NOT_GIVEN, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(int viewId, int titleResId, int messageResId, float scale, ShowcaseView.ConfigOptions configOptions) {
			this(null, viewId, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, scale, configOptions);
		}

		public ItemViewProperties(View view, int titleResId, int messageResId, float scale, ShowcaseView.ConfigOptions configOptions) {
			this(view, ID_NOT_GIVEN, null, titleResId, null, messageResId, ID_NOT_IN_ACTIONBAR, scale, configOptions);
		}

		public ItemViewProperties(int viewId, String title, String message, float scale, ShowcaseView.ConfigOptions configOptions) {
			this(null, viewId, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, scale, configOptions);
		}

		public ItemViewProperties(View view, String title, String message, float scale, ShowcaseView.ConfigOptions configOptions) {
			this(view, ID_NOT_GIVEN, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, ID_NOT_IN_ACTIONBAR, scale, configOptions);
		}

		public ItemViewProperties(int viewId, int titleResId, int messageResId, int itemType, ShowcaseView.ConfigOptions configOptions) {
			this(null, viewId, null, titleResId, null, messageResId, itemType, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(View view, int titleResId, int messageResId, int itemType, ShowcaseView.ConfigOptions configOptions) {
			this(view, ID_NOT_GIVEN, null, titleResId, null, messageResId, itemType, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(int viewId, String title, String message, int itemType, ShowcaseView.ConfigOptions configOptions) {
			this(null, viewId, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, itemType, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(View view, String title, String message, int itemType, ShowcaseView.ConfigOptions configOptions) {
			this(view, ID_NOT_GIVEN, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, itemType, DEFAULT_SCALE, configOptions);
		}

		public ItemViewProperties(int viewId, int titleResId, int messageResId, int itemType, float scale, ShowcaseView.ConfigOptions configOptions) {
			this(null, viewId, null, titleResId, null, messageResId, itemType, scale, configOptions);
		}

		public ItemViewProperties(int viewId, String title, String message, int itemType, float scale, ShowcaseView.ConfigOptions configOptions) {
			this(null, viewId, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, itemType, scale, configOptions);
		}

		public ItemViewProperties(View view, int titleResId, int messageResId, int itemType, float scale, ShowcaseView.ConfigOptions configOptions) {
			this(view, ID_NOT_GIVEN, null, titleResId, null, messageResId, itemType, scale, configOptions);
		}

		public ItemViewProperties(View view, String title, String message, int itemType, float scale, ShowcaseView.ConfigOptions configOptions) {
			this(view, ID_NOT_GIVEN, title, ID_NOT_GIVEN, message, ID_NOT_GIVEN, itemType, scale, configOptions);
		}

		public ItemViewProperties(View view, int viewId, String title, int titleResId, String message, int messageResId,
				int itemType, float scale, ShowcaseView.ConfigOptions configOptions) {
			mmView = view;
			mmViewId = viewId;
			mmTitle = title;
			mmTitleResId = titleResId;
			mmMessage = message;
			mmMessageResId = messageResId;
			mmItemType = itemType;
			mmScale = scale;
			mmConfigOptions = configOptions;
		}
	}
}