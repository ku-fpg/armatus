package pl.polidea.treelistview;

import android.content.Context;
import android.content.res.TypedArray;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.Gravity;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ListAdapter;
import android.widget.ListView;

/**
 * Tree view, expandable multi-level.
 * 
 * <pre>
 * attr ref pl.polidea.treeview.R.styleable#TreeListView_collapsible
 * attr ref pl.polidea.treeview.R.styleable#TreeListView_src_expanded
 * attr ref pl.polidea.treeview.R.styleable#TreeListView_src_collapsed
 * attr ref pl.polidea.treeview.R.styleable#TreeListView_indent_width
 * attr ref pl.polidea.treeview.R.styleable#TreeListView_handle_trackball_press
 * attr ref pl.polidea.treeview.R.styleable#TreeListView_indicator_gravity
 * attr ref pl.polidea.treeview.R.styleable#TreeListView_indicator_background
 * attr ref pl.polidea.treeview.R.styleable#TreeListView_row_background
 * </pre>
 */
public class TreeListView extends ListView {
	private static final int DEFAULT_COLLAPSED_RESOURCE = R.drawable.collapsed;
	private static final int DEFAULT_EXPANDED_RESOURCE = R.drawable.expanded;
	private static final int DEFAULT_INDENT = 0;
	private static final int DEFAULT_GRAVITY = Gravity.LEFT
			| Gravity.CENTER_VERTICAL;
	private Drawable mExpandedDrawable;
	private Drawable mCollapsedDrawable;
	private Drawable mRowBackgroundDrawable;
	private Drawable mIndicatorBackgroundDrawable;
	private int mIndentWidth = 0;
	private int mIndicatorGravity = 0;
	private AbstractTreeListAdapter<?> mTreeAdapter;
	private boolean mCollapsible;
	private boolean mHandleTrackballPress;

	public TreeListView(final Context context, final AttributeSet attrs) {
		this(context, attrs, R.style.treeViewListStyle);
	}

	public TreeListView(final Context context) {
		this(context, null);
	}

	public TreeListView(final Context context, final AttributeSet attrs,
			final int defStyle) {
		super(context, attrs, defStyle);
		parseAttributes(context, attrs);
	}

	private void parseAttributes(final Context context, final AttributeSet attrs) {
		final TypedArray a = context.obtainStyledAttributes(attrs,
				R.styleable.TreeListView);
		int expandedRes = a.getResourceId(R.styleable.TreeListView_src_expanded, -1);
		if (expandedRes != -1) {
			mExpandedDrawable = getResources().getDrawable(expandedRes);
		}
		if (mExpandedDrawable == null) {
			mExpandedDrawable = context.getResources().getDrawable(
					DEFAULT_EXPANDED_RESOURCE);
		}
		int collapsedRes = a.getResourceId(R.styleable.TreeListView_src_collapsed, -1);
		if (collapsedRes != -1) {
			mCollapsedDrawable = getResources().getDrawable(collapsedRes);
		}
		if (mCollapsedDrawable == null) {
			mCollapsedDrawable = context.getResources().getDrawable(
					DEFAULT_COLLAPSED_RESOURCE);
		}
		mIndentWidth = a.getDimensionPixelSize(
				R.styleable.TreeListView_indent_width, DEFAULT_INDENT);
		mIndicatorGravity = a.getInteger(
				R.styleable.TreeListView_indicator_gravity, DEFAULT_GRAVITY);
		mIndicatorBackgroundDrawable = a
				.getDrawable(R.styleable.TreeListView_indicator_background);
		mRowBackgroundDrawable = a
				.getDrawable(R.styleable.TreeListView_row_background);
		mCollapsible = a.getBoolean(R.styleable.TreeListView_collapsible, true);
		mHandleTrackballPress = a.getBoolean(
				R.styleable.TreeListView_handle_trackball_press, true);
		a.recycle();
	}

	@Override
	public void setAdapter(final ListAdapter adapter) {
		if (!(adapter instanceof AbstractTreeListAdapter)) {
			throw new TreeConfigurationException(
					"The adapter is not of TreeViewAdapter type");
		}
		mTreeAdapter = (AbstractTreeListAdapter< ? >) adapter;
		syncAdapter();
		super.setAdapter(mTreeAdapter);
	}

	private void syncAdapter() {
		mTreeAdapter.setCollapsedDrawable(mCollapsedDrawable);
		mTreeAdapter.setExpandedDrawable(mExpandedDrawable);
		mTreeAdapter.setIndicatorGravity(mIndicatorGravity);
		mTreeAdapter.setIndentWidth(mIndentWidth);
		mTreeAdapter.setIndicatorBackgroundDrawable(mIndicatorBackgroundDrawable);
		mTreeAdapter.setRowBackgroundDrawable(mRowBackgroundDrawable);
		mTreeAdapter.setCollapsible(mCollapsible);
		if (mHandleTrackballPress) {
			setOnItemClickListener(new OnItemClickListener() {
				@Override
				public void onItemClick(final AdapterView< ? > parent,
						final View view, final int position, final long id) {
					mTreeAdapter.handleItemClick(view, view.getTag());
				}
			});
		} else {
			setOnClickListener(null);
		}

	}

	public void setExpandedDrawable(final Drawable expandedDrawable) {
		mExpandedDrawable = expandedDrawable;
		syncAdapter();
		mTreeAdapter.refresh();
	}

	public void setCollapsedDrawable(final Drawable collapsedDrawable) {
		mCollapsedDrawable = collapsedDrawable;
		syncAdapter();
		mTreeAdapter.refresh();
	}

	public void setRowBackgroundDrawable(final Drawable rowBackgroundDrawable) {
		mRowBackgroundDrawable = rowBackgroundDrawable;
		syncAdapter();
		mTreeAdapter.refresh();
	}

	public void setIndicatorBackgroundDrawable(
			final Drawable indicatorBackgroundDrawable) {
		mIndicatorBackgroundDrawable = indicatorBackgroundDrawable;
		syncAdapter();
		mTreeAdapter.refresh();
	}

	public void setIndentWidth(final int indentWidth) {
		mIndentWidth = indentWidth;
		syncAdapter();
		mTreeAdapter.refresh();
	}

	public void setIndicatorGravity(final int indicatorGravity) {
		mIndicatorGravity = indicatorGravity;
		syncAdapter();
		mTreeAdapter.refresh();
	}

	public void setCollapsible(final boolean collapsible) {
		mCollapsible = collapsible;
		syncAdapter();
		mTreeAdapter.refresh();
	}

	public void setHandleTrackballPress(final boolean handleTrackballPress) {
		mHandleTrackballPress = handleTrackballPress;
		syncAdapter();
		mTreeAdapter.refresh();
	}

	public Drawable getExpandedDrawable() {
		return mExpandedDrawable;
	}

	public Drawable getCollapsedDrawable() {
		return mCollapsedDrawable;
	}

	public Drawable getRowBackgroundDrawable() {
		return mRowBackgroundDrawable;
	}

	public Drawable getIndicatorBackgroundDrawable() {
		return mIndicatorBackgroundDrawable;
	}

	public int getIndentWidth() {
		return mIndentWidth;
	}

	public int getIndicatorGravity() {
		return mIndicatorGravity;
	}

	public boolean isCollapsible() {
		return mCollapsible;
	}

	public boolean isHandleTrackballPress() {
		return mHandleTrackballPress;
	}

}
