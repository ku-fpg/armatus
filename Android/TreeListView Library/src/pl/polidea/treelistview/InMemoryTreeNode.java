package pl.polidea.treelistview;

import java.io.Serializable;
import java.util.LinkedList;
import java.util.List;

/**
 * Node. It is package protected so that it cannot be used outside.
 * 
 * @param <T>
 *            type of the identifier used by the tree
 */
class InMemoryTreeNode<T> implements Serializable {
	private static final long serialVersionUID = 1L;
	private final T mId;
	private final T mParent;
	private final int mLevel;
	private boolean mVisible = true;
	private final List<InMemoryTreeNode<T>> mChildren = new LinkedList<InMemoryTreeNode<T>>();
	private List<T> mChildIdListCache = null;

	public InMemoryTreeNode(final T id, final T parent, final int level,
			final boolean visible) {
		mId = id;
		mParent = parent;
		mLevel = level;
		mVisible = visible;
	}

	public int indexOf(final T id) {
		return getChildIdList().indexOf(id);
	}

	/**
	 * Cache is built lasily only if needed. The cache is cleaned on any
	 * structure change for that node!).
	 * 
	 * @return list of ids of children
	 */
	public synchronized List<T> getChildIdList() {
		if (mChildIdListCache == null) {
			mChildIdListCache = new LinkedList<T>();
			for (final InMemoryTreeNode<T> n : mChildren) {
				mChildIdListCache.add(n.getId());
			}
		}
		return mChildIdListCache;
	}

	public boolean isVisible() {
		return mVisible;
	}

	public void setVisible(final boolean visible) {
		mVisible = visible;
	}

	public int getChildrenListSize() {
		return mChildren.size();
	}

	public synchronized InMemoryTreeNode<T> add(final int index, final T child,
			final boolean visible) {
		mChildIdListCache = null;
		// Note! top levell children are always visible (!)
		final InMemoryTreeNode<T> newNode = new InMemoryTreeNode<T>(child,
				getId(), getLevel() + 1, getId() == null || visible);
		mChildren.add(index, newNode);
		return newNode;
	}

	/**
	 * Note. This method should technically return unmodifiable collection, but
	 * for performance reason on small devices we do not do it.
	 * 
	 * @return children list
	 */
	public List<InMemoryTreeNode<T>> getChildren() {
		return mChildren;
	}

	public synchronized void clearChildren() {
		mChildren.clear();
		mChildIdListCache = null;
	}

	public synchronized void removeChild(final T child) {
		final int childIndex = indexOf(child);
		if (childIndex != -1) {
			mChildren.remove(childIndex);
			mChildIdListCache = null;
		}
	}

	@Override
	public String toString() {
		return "InMemoryTreeNode [id=" + getId() + ", parent=" + getParent()
				+ ", level=" + getLevel() + ", visible=" + mVisible
				+ ", children=" + mChildren + ", childIdListCache="
				+ mChildIdListCache + ']';
	}

	T getId() {
		return mId;
	}

	T getParent() {
		return mParent;
	}

	int getLevel() {
		return mLevel;
	}

}