package edu.kufpg.armatus.treelistview;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import pl.polidea.treelistview.InMemoryTreeStateManager;
import pl.polidea.treelistview.TreeBuilder;
import pl.polidea.treelistview.TreeListView;
import pl.polidea.treelistview.TreeNodeInfo;
import pl.polidea.treelistview.TreeStateManager;
import android.os.Bundle;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView.AdapterContextMenuInfo;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.util.BundleUtils;

/**
 * Demo activity showing how the tree view can be used.
 * 
 */
public class TreeListViewDemo extends BaseActivity {
	private static final String TAG = TreeListViewDemo.class.getSimpleName();
	private static final boolean DEBUG = false;

	private enum TreeType implements Serializable { SIMPLE,	FANCY };
	private final Set<Long> mSelected = new HashSet<Long>();
	private TreeListView mTreeView;
	private static final int[] DEMO_NODES = new int[] { 0, 0, 1, 1, 1, 2, 2, 1,
		1, 2, 1, 0, 0, 0, 1, 2, 3, 2, 0, 0, 1, 2, 0, 1, 2, 0, 1 };
	private static final int LEVEL_NUMBER = 4;
	private TreeStateManager<Long> mManager = null;
	private FancyColoredVariousSizesAdapter mFancyAdapter;
	private SimpleStandardAdapter mSimpleAdapter;
	private TreeType mTreeType;
	private boolean mCollapsible, mRecursive;

	@SuppressWarnings("unchecked")
	@Override
	public void onCreate(final Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		TreeType newTreeType = null;
		boolean newCollapsible;
		if (savedInstanceState == null) {
			mManager = new InMemoryTreeStateManager<Long>();
			final TreeBuilder<Long> treeBuilder = new TreeBuilder<Long>(mManager);
			for (int i = 0; i < DEMO_NODES.length; i++) {
				treeBuilder.sequentiallyAddNextNode((long) i, DEMO_NODES[i]);
			}
			if (DEBUG) Log.d(TAG, mManager.toString());
			newTreeType = TreeType.SIMPLE;
			newCollapsible = false;
			mRecursive = false;
		} else {
			mManager = (TreeStateManager<Long>) savedInstanceState
					.getSerializable("treeManager");
			newTreeType = BundleUtils.getEnum(savedInstanceState, "treeType");
			newCollapsible = savedInstanceState.getBoolean("collapsible");
			mRecursive = savedInstanceState.getBoolean("recursive");
		}
		setContentView(R.layout.tree_list_view_demo);
		mTreeView = (TreeListView) findViewById(R.id.mainTreeView);
		mFancyAdapter = new FancyColoredVariousSizesAdapter(this, mSelected,
				mManager, LEVEL_NUMBER);
		mSimpleAdapter = new SimpleStandardAdapter(this, mSelected, mManager,
				LEVEL_NUMBER);
		setTreeAdapter(newTreeType);
		setCollapsible(newCollapsible);
		registerForContextMenu(mTreeView);
	}

	@Override
	protected void onSaveInstanceState(final Bundle outState) {
		outState.putSerializable("treeManager", mManager);
		BundleUtils.putEnum(outState, "treeType", mTreeType);
		outState.putBoolean("collapsible", mCollapsible);
		super.onSaveInstanceState(outState);
	}

	protected final void setTreeAdapter(final TreeType newTreeType) {
		mTreeType = newTreeType;
		switch (newTreeType) {
		case SIMPLE:
			mTreeView.setAdapter(mSimpleAdapter);
			break;
		case FANCY:
			mTreeView.setAdapter(mFancyAdapter);
			break;
		default:
			mTreeView.setAdapter(mSimpleAdapter);
		}
	}

	protected final void setCollapsible(final boolean newCollapsible) {
		mCollapsible = newCollapsible;
		mTreeView.setCollapsible(mCollapsible);
	}

	@Override
	public boolean onCreateOptionsMenu(final Menu menu) {
		final MenuInflater inflater = getMenuInflater();
		inflater.inflate(R.menu.tree_list_view_menu, menu);
		return true;
	}

	@Override
	public boolean onPrepareOptionsMenu(final Menu menu) {
		menu.findItem(R.id.expand_all_menu_item).setVisible(mCollapsible);
		menu.findItem(R.id.collapse_all_menu_item).setVisible(mCollapsible);
		menu.findItem(R.id.recursive_menu_item).setVisible(mCollapsible);
		return super.onPrepareOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		switch (item.getItemId()) {
		case R.id.simple_menu_item:
			setTreeAdapter(TreeType.SIMPLE);
			return true;
		case R.id.fancy_menu_item:
			setTreeAdapter(TreeType.FANCY);
			return true;
		case R.id.expand_all_menu_item:
			mManager.expandChildren(null, mRecursive);
			return true;
		case R.id.collapse_all_menu_item:
			mManager.collapseChildren(null, mRecursive);
			return true;
		case R.id.collapsible_menu_item:
			setCollapsible(!mCollapsible);
			item.setChecked(mCollapsible);
			return true;
		case R.id.recursive_menu_item:
			mRecursive = !mRecursive;
			item.setChecked(mRecursive);
		}
		return false;
	}

	@Override
	public void onCreateContextMenu(final ContextMenu menu, final View v,
			final ContextMenuInfo menuInfo) {
		final AdapterContextMenuInfo adapterInfo = (AdapterContextMenuInfo) menuInfo;
		final long id = adapterInfo.id;
		final TreeNodeInfo<Long> info = mManager.getNodeInfo(id);
		final MenuInflater menuInflater = getMenuInflater();
		menuInflater.inflate(R.menu.tree_list_view_context_menu, menu);
		if (info.isWithChildren() && mCollapsible) {
			if (info.isExpanded()) {
				menu.findItem(R.id.context_menu_expand_item).setVisible(false);
				menu.findItem(R.id.context_menu_expand_all).setVisible(false);
			} else {
				menu.findItem(R.id.context_menu_collapse_item).setVisible(false);
				menu.findItem(R.id.context_menu_collapse_all).setVisible(false);
			}
		} else {
			menu.findItem(R.id.context_menu_expand_item).setVisible(false);
			menu.findItem(R.id.context_menu_expand_all).setVisible(false);
			menu.findItem(R.id.context_menu_collapse_item).setVisible(false);
			menu.findItem(R.id.context_menu_collapse_all).setVisible(false);
		}
		super.onCreateContextMenu(menu, v, menuInfo);
	}

	@Override
	public boolean onContextItemSelected(final MenuItem item) {
		final AdapterContextMenuInfo info = (AdapterContextMenuInfo) item
				.getMenuInfo();
		final long id = info.id;
		switch (item.getItemId()) {
		case R.id.context_menu_expand_item:
			mManager.expandChildren(id, false);
			return true;
		case R.id.context_menu_expand_all:
			mManager.expandChildren(id, true);
			return true;
		case R.id.context_menu_collapse_item:
			mManager.collapseChildren(id, false);
			return true;
		case R.id.context_menu_collapse_all:
			mManager.collapseChildren(id, true);
			return true;
		case R.id.context_menu_delete:
			mManager.removeNodeRecursively(id);
			return true;
		}
		return super.onContextItemSelected(item);
	}
}