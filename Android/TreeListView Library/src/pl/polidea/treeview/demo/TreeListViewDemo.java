package pl.polidea.treeview.demo;

import java.io.Serializable;
import java.util.HashSet;
import java.util.Set;

import pl.polidea.treeview.InMemoryTreeStateManager;
import pl.polidea.treeview.R;
import pl.polidea.treeview.TreeBuilder;
import pl.polidea.treeview.TreeNodeInfo;
import pl.polidea.treeview.TreeStateManager;
import pl.polidea.treeview.TreeListView;
import android.app.Activity;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView.AdapterContextMenuInfo;

/**
 * Demo activity showing how the tree view can be used.
 * 
 */
public class TreeListViewDemo extends Activity {
	private enum TreeType implements Serializable { SIMPLE,	FANCY };

	private final Set<Long> mSelected = new HashSet<Long>();
	//private static final String TAG = TreeListViewDemo.class.getSimpleName();
	private TreeListView mTreeView;
	private static final int[] DEMO_NODES = new int[] { 0, 0, 1, 1, 1, 2, 2, 1,
		1, 2, 1, 0, 0, 0, 1, 2, 3, 2, 0, 0, 1, 2, 0, 1, 2, 0, 1 };
	private static final int LEVEL_NUMBER = 4;
	private TreeStateManager<Long> mManager = null;
	private FancyColouredVariousSizesAdapter mFancyAdapter;
	private SimpleStandardAdapter mSimpleAdapter;
	private TreeType mTreeType;
	private boolean mCollapsible;

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
			//Log.d(TAG, mManager.toString());
			newTreeType = TreeType.SIMPLE;
			newCollapsible = false;
		} else {
			mManager = (TreeStateManager<Long>) savedInstanceState
					.getSerializable("treeManager");
			newTreeType = (TreeType) savedInstanceState
					.getSerializable("treeType");
			newCollapsible = savedInstanceState.getBoolean("collapsible");
		}
		setContentView(R.layout.main_demo);
		mTreeView = (TreeListView) findViewById(R.id.mainTreeView);
		mFancyAdapter = new FancyColouredVariousSizesAdapter(this, mSelected,
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
		outState.putSerializable("treeType", mTreeType);
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
		inflater.inflate(R.menu.main_menu, menu);
		return true;
	}

	@Override
	public boolean onPrepareOptionsMenu(final Menu menu) {
		final MenuItem collapsibleMenu = menu
				.findItem(R.id.collapsible_menu_item);
		if (mCollapsible) {
			collapsibleMenu.setTitle(R.string.collapsible_menu_disable);
			collapsibleMenu.setTitleCondensed(getResources().getString(
					R.string.collapsible_condensed_disable));
		} else {
			collapsibleMenu.setTitle(R.string.collapsible_menu_enable);
			collapsibleMenu.setTitleCondensed(getResources().getString(
					R.string.collapsible_condensed_enable));
		}
		return super.onPrepareOptionsMenu(menu);
	}

	@Override
	public boolean onOptionsItemSelected(final MenuItem item) {
		if (item.getItemId() == R.id.simple_menu_item) {
			setTreeAdapter(TreeType.SIMPLE);
		} else if (item.getItemId() == R.id.fancy_menu_item) {
			setTreeAdapter(TreeType.FANCY);
		} else if (item.getItemId() == R.id.collapsible_menu_item) {
			setCollapsible(!mCollapsible);
		} else if (item.getItemId() == R.id.expand_all_menu_item) {
			mManager.expandEverythingBelow(null);
		} else if (item.getItemId() == R.id.collapse_all_menu_item) {
			mManager.collapseChildren(null);
		} else {
			return false;
		}
		return true;
	}

	@Override
	public void onCreateContextMenu(final ContextMenu menu, final View v,
			final ContextMenuInfo menuInfo) {
		final AdapterContextMenuInfo adapterInfo = (AdapterContextMenuInfo) menuInfo;
		final long id = adapterInfo.id;
		final TreeNodeInfo<Long> info = mManager.getNodeInfo(id);
		final MenuInflater menuInflater = getMenuInflater();
		menuInflater.inflate(R.menu.context_menu, menu);
		if (info.isWithChildren()) {
			if (info.isExpanded()) {
				menu.findItem(R.id.context_menu_expand_item).setVisible(false);
				menu.findItem(R.id.context_menu_expand_all).setVisible(false);
			} else {
				menu.findItem(R.id.context_menu_collapse).setVisible(false);
			}
		} else {
			menu.findItem(R.id.context_menu_expand_item).setVisible(false);
			menu.findItem(R.id.context_menu_expand_all).setVisible(false);
			menu.findItem(R.id.context_menu_collapse).setVisible(false);
		}
		super.onCreateContextMenu(menu, v, menuInfo);
	}

	@Override
	public boolean onContextItemSelected(final MenuItem item) {
		final AdapterContextMenuInfo info = (AdapterContextMenuInfo) item
				.getMenuInfo();
		final long id = info.id;
		if (item.getItemId() == R.id.context_menu_collapse) {
			mManager.collapseChildren(id);
			return true;
		} else if (item.getItemId() == R.id.context_menu_expand_all) {
			mManager.expandEverythingBelow(id);
			return true;
		} else if (item.getItemId() == R.id.context_menu_expand_item) {
			mManager.expandDirectChildren(id);
			return true;
		} else if (item.getItemId() == R.id.context_menu_delete) {
			mManager.removeNodeRecursively(id);
			return true;
		} else {
			return super.onContextItemSelected(item);
		}
	}
}