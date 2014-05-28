package pl.polidea.treelistview;

import android.database.DataSetObserver;
import android.util.Log;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * In-memory manager of tree state.
 * 
 * @param <T>
 *            type of identifier
 */
public class InMemoryTreeStateManager<T> implements TreeStateManager<T> {
	private static final String TAG = InMemoryTreeStateManager.class.getSimpleName();
	private static final boolean DEBUG = false;
	private static final long serialVersionUID = 1L;
	private final Map<T, InMemoryTreeNode<T>> mAllNodes = new HashMap<T, InMemoryTreeNode<T>>();
	private final InMemoryTreeNode<T> mTopSentinel = new InMemoryTreeNode<T>(
			null, null, -1, true);
	private transient List<T> mVisibleListCache = null; // lazy initialized
	private transient List<T> mUnmodifiableVisibleList = null;
	private boolean mCollapsible;
	private boolean mVisibleByDefault = true;
	private final transient Set<DataSetObserver> mObservers = new HashSet<DataSetObserver>();

	private synchronized void internalDataSetChanged() {
		mVisibleListCache = null;
		mUnmodifiableVisibleList = null;
		for (final DataSetObserver observer : mObservers) {
			observer.onChanged();
		}
	}

	/**
	 * If true new nodes are visible by default.
	 * 
	 * @param visibleByDefault
	 *            if true, then newly added nodes are expanded by default
	 */
	public void setVisibleByDefault(final boolean visibleByDefault) {
		mVisibleByDefault = visibleByDefault;
	}

	private InMemoryTreeNode<T> getNodeFromTreeOrThrow(final T id) {
		if (id == null) {
			throw new NodeNotInTreeException("(null)");
		}
		final InMemoryTreeNode<T> node = mAllNodes.get(id);
		if (node == null) {
			throw new NodeNotInTreeException(id.toString());
		}
		return node;
	}

	private InMemoryTreeNode<T> getNodeFromTreeOrThrowAllowRoot(final T id) {
		if (id == null) {
			return mTopSentinel;
		}
		return getNodeFromTreeOrThrow(id);
	}

	private void expectNodeNotInTreeYet(final T id) {
		final InMemoryTreeNode<T> node = mAllNodes.get(id);
		if (node != null) {
			throw new NodeAlreadyInTreeException(id.toString(), node.toString());
		}
	}

	@Override
	public synchronized TreeNodeInfo<T> getNodeInfo(final T id) {
		final InMemoryTreeNode<T> node = getNodeFromTreeOrThrow(id);
		final List<InMemoryTreeNode<T>> children = node.getChildren();
		boolean expanded = false;
		if (!children.isEmpty() && children.get(0).isVisible()) {
			expanded = true;
		}
		return new TreeNodeInfo<T>(id, node.getLevel(), !children.isEmpty(),
				node.isVisible(), expanded);
	}

	@Override
	public synchronized List<T> getChildren(final T id) {
		final InMemoryTreeNode<T> node = getNodeFromTreeOrThrowAllowRoot(id);
		return node.getChildIdList();
	}

	@Override
	public synchronized T getParent(final T id) {
		final InMemoryTreeNode<T> node = getNodeFromTreeOrThrowAllowRoot(id);
		return node.getParent();
	}

	private boolean getChildrenVisibility(final InMemoryTreeNode<T> node) {
		boolean visibility;
		final List<InMemoryTreeNode<T>> children = node.getChildren();
		if (children.isEmpty()) {
			visibility = mVisibleByDefault;
		} else {
			visibility = children.get(0).isVisible();
		}
		return visibility;
	}

	@Override
	public synchronized void addBeforeChild(final T parent, final T newChild,
			final T beforeChild) {
		expectNodeNotInTreeYet(newChild);
		final InMemoryTreeNode<T> node = getNodeFromTreeOrThrowAllowRoot(parent);
		final boolean visibility = getChildrenVisibility(node);
		// top nodes are always expanded.
		if (beforeChild == null) {
			final InMemoryTreeNode<T> added = node.add(0, newChild, visibility);
			mAllNodes.put(newChild, added);
		} else {
			final int index = node.indexOf(beforeChild);
			final InMemoryTreeNode<T> added = node.add(index == -1 ? 0 : index,
					newChild, visibility);
			mAllNodes.put(newChild, added);
		}
		if (visibility) {
			internalDataSetChanged();
		}
	}

	@Override
	public synchronized void addAfterChild(final T parent, final T newChild,
			final T afterChild) {
		expectNodeNotInTreeYet(newChild);
		final InMemoryTreeNode<T> node = getNodeFromTreeOrThrowAllowRoot(parent);
		final boolean visibility = getChildrenVisibility(node);
		if (afterChild == null) {
			final InMemoryTreeNode<T> added = node.add(
					node.getChildrenListSize(), newChild, visibility);
			mAllNodes.put(newChild, added);
		} else {
			final int index = node.indexOf(afterChild);
			final InMemoryTreeNode<T> added = node.add(
					index == -1 ? node.getChildrenListSize() : index + 1, newChild,
							visibility);
			mAllNodes.put(newChild, added);
		}
		if (visibility) {
			internalDataSetChanged();
		}
	}

	@Override
	public synchronized void removeNodeRecursively(final T id) {
		final InMemoryTreeNode<T> node = getNodeFromTreeOrThrowAllowRoot(id);
		final boolean visibleNodeChanged = removeNodeRecursively(node);
		final T parent = node.getParent();
		final InMemoryTreeNode<T> parentNode = getNodeFromTreeOrThrowAllowRoot(parent);
		parentNode.removeChild(id);
		if (visibleNodeChanged) {
			internalDataSetChanged();
		}
	}

	private boolean removeNodeRecursively(final InMemoryTreeNode<T> node) {
		boolean visibleNodeChanged = false;
		for (final InMemoryTreeNode<T> child : node.getChildren()) {
			if (removeNodeRecursively(child)) {
				visibleNodeChanged = true;
			}
		}
		node.clearChildren();
		if (node.getId() != null) {
			mAllNodes.remove(node.getId());
			if (node.isVisible()) {
				visibleNodeChanged = true;
			}
		}
		return visibleNodeChanged;
	}

	private void setChildrenVisibility(final InMemoryTreeNode<T> node,
			final boolean visible, final boolean recursive) {
		for (final InMemoryTreeNode<T> child : node.getChildren()) {
			child.setVisible(visible);
			if (recursive) {
				setChildrenVisibility(child, visible, true);
			}
		}
	}

	@Override
	public synchronized void setCollapsible(boolean collapsible) {
		if (!collapsible) {
			expandChildren(null, true);
		}
		mCollapsible = collapsible;
	}

	@Override
	public synchronized void expandChildren(final T id, boolean recursive) {
		if (DEBUG) Log.d(TAG, "Expanding " + (recursive ? "all children below " : "direct children of ") + id);
		if (mCollapsible) {
			final InMemoryTreeNode<T> node = getNodeFromTreeOrThrowAllowRoot(id);
			if (node == mTopSentinel) {
				for (final InMemoryTreeNode<T> n : mTopSentinel.getChildren()) {
					setChildrenVisibility(n, true, recursive);
				}
			}
			setChildrenVisibility(node, true, recursive);
			internalDataSetChanged();
		}
	}

	@Override
	public synchronized void collapseChildren(final T id, boolean recursive) {
		if (DEBUG) Log.d(TAG, "Collapsing " + (recursive ? "all children below " : "direct children of ") + id);
		if (mCollapsible) {
			final InMemoryTreeNode<T> node = getNodeFromTreeOrThrowAllowRoot(id);
			if (node == mTopSentinel) {
				for (final InMemoryTreeNode<T> n : mTopSentinel.getChildren()) {
					setChildrenVisibility(n, false, recursive);
				}
			} else {
				setChildrenVisibility(node, false, recursive);
			}
			internalDataSetChanged();
		}
	}

	@Override
	public synchronized T getNextSibling(final T id) {
		final T parent = getParent(id);
		final InMemoryTreeNode<T> parentNode = getNodeFromTreeOrThrowAllowRoot(parent);
		boolean returnNext = false;
		for (final InMemoryTreeNode<T> child : parentNode.getChildren()) {
			if (returnNext) {
				return child.getId();
			}
			if (child.getId().equals(id)) {
				returnNext = true;
			}
		}
		return null;
	}

	@Override
	public synchronized T getPreviousSibling(final T id) {
		final T parent = getParent(id);
		final InMemoryTreeNode<T> parentNode = getNodeFromTreeOrThrowAllowRoot(parent);
		T previousSibling = null;
		for (final InMemoryTreeNode<T> child : parentNode.getChildren()) {
			if (child.getId().equals(id)) {
				return previousSibling;
			}
			previousSibling = child.getId();
		}
		return null;
	}

	@Override
	public synchronized boolean isCollapsible() {
		return mCollapsible;
	}

	@Override
	public synchronized boolean isInTree(final T id) {
		return mAllNodes.containsKey(id);
	}

	@Override
	public synchronized int getVisibleCount() {
		return getVisibleList().size();
	}

	@Override
	public synchronized List<T> getVisibleList() {
		T currentId = null;
		if (mVisibleListCache == null) {
			mVisibleListCache = new ArrayList<T>(mAllNodes.size());
			do {
				currentId = getNextVisible(currentId);
				if (currentId == null) {
					break;
				} else {
					mVisibleListCache.add(currentId);
				}
			} while (true);
		}
		if (mUnmodifiableVisibleList == null) {
			mUnmodifiableVisibleList = Collections
					.unmodifiableList(mVisibleListCache);
		}
		return mUnmodifiableVisibleList;
	}

	public synchronized T getNextVisible(final T id) {
		final InMemoryTreeNode<T> node = getNodeFromTreeOrThrowAllowRoot(id);
		if (!node.isVisible()) {
			return null;
		}
		final List<InMemoryTreeNode<T>> children = node.getChildren();
		if (!children.isEmpty()) {
			final InMemoryTreeNode<T> firstChild = children.get(0);
			if (firstChild.isVisible()) {
				return firstChild.getId();
			}
		}
		final T sibl = getNextSibling(id);
		if (sibl != null) {
			return sibl;
		}
		T parent = node.getParent();
		do {
			if (parent == null) {
				return null;
			}
			final T parentSibling = getNextSibling(parent);
			if (parentSibling != null) {
				return parentSibling;
			}
			parent = getNodeFromTreeOrThrow(parent).getParent();
		} while (true);
	}

	@Override
	public synchronized void registerDataSetObserver(
			final DataSetObserver observer) {
		mObservers.add(observer);
	}

	@Override
	public synchronized void unregisterDataSetObserver(
			final DataSetObserver observer) {
		mObservers.remove(observer);
	}

	@Override
	public int getLevel(final T id) {
		return getNodeFromTreeOrThrow(id).getLevel();
	}

	@Override
	public Integer[] getHierarchyDescription(final T id) {
		final int level = getLevel(id);
		final Integer[] hierarchy = new Integer[level + 1];
		int currentLevel = level;
		T currentId = id;
		T parent = getParent(currentId);
		while (currentLevel >= 0) {
			hierarchy[currentLevel--] = getChildren(parent).indexOf(currentId);
			currentId = parent;
			parent = getParent(parent);
		}
		return hierarchy;
	}

	private void appendToSb(final StringBuilder sb, final T id) {
		if (id != null) {
			final TreeNodeInfo<T> node = getNodeInfo(id);
			final int indent = node.getLevel() * 4;
			final char[] indentString = new char[indent];
			Arrays.fill(indentString, ' ');
			sb.append(indentString);
			sb.append(node);
			sb.append(Arrays.asList(getHierarchyDescription(id)));
			sb.append('\n');
		}
		final List<T> children = getChildren(id);
		for (final T child : children) {
			appendToSb(sb, child);
		}
	}

	@Override
	public synchronized String toString() {
		final StringBuilder sb = new StringBuilder();
		appendToSb(sb, null);
		return sb.toString();
	}

	@Override
	public synchronized void clear() {
		mAllNodes.clear();
		mTopSentinel.clearChildren();
		internalDataSetChanged();
	}

	@Override
	public void refresh() {
		internalDataSetChanged();
	}

}
