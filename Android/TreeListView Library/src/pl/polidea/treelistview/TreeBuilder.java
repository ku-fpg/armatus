package pl.polidea.treelistview;

import android.util.Log;

/**
 * Allows to build tree easily in sequential mode (you have to know levels of
 * all the tree elements upfront). You should rather use this class rather than
 * manager if you build initial tree from some external data source.
 * <p>
 * Note, that all ids must be unique. IDs are used to find nodes in the whole
 * tree, so they cannot repeat even if they are in different 
 * sub-trees. 
 * 
 * @param <T>
 */
public class TreeBuilder<T> {
	private static final String TAG = TreeBuilder.class.getSimpleName();
	private static final boolean DEBUG = false;

	private final TreeStateManager<T> mManager;

	private T mLastAddedId = null;
	private int mLastLevel = -1;

	public TreeBuilder(final TreeStateManager<T> manager) {
		mManager = manager;
	}

	public void clear() {
		mManager.clear();
	}

	/**
	 * Adds new relation to existing tree. Child is set as the last child of the
	 * parent node. Parent has to already exist in the tree, child cannot yet
	 * exist. This method is mostly useful in case you add entries layer by
	 * layer - i.e. first top level entries, then children for all parents, then
	 * grand-children and so on.
	 * 
	 * @param parent
	 *            parent id
	 * @param child
	 *            child id
	 */
	public synchronized void addRelation(final T parent, final T child) {
		if (DEBUG) Log.d(TAG, "Adding relation parent:" + parent + " -> child: " + child);
		mManager.addAfterChild(parent, child, null);
		mLastAddedId = child;
		mLastLevel = mManager.getLevel(child);
	}

	/**
	 * Adds sequentially new node. Using this method is the simplest way of
	 * building tree - if you have all the elements in the sequence as they
	 * should be displayed in fully-expanded tree. You can combine it with add
	 * relation - for example you can add information about few levels using
	 * {@link addRelation} and then after the right level is added as parent,
	 * you can continue adding them using sequential operation.
	 * 
	 * @param id
	 *            id of the node
	 * @param level
	 *            its level
	 */
	public synchronized void sequentiallyAddNextNode(final T id, final int level) {
		if (DEBUG) Log.d(TAG, "Adding sequential node " + id + " at level " + level);
		if (mLastAddedId == null) {
			addNodeToParentOneLevelDown(null, id, level);
		} else {
			if (level <= mLastLevel) {
				final T parent = findParentAtLevel(mLastAddedId, level - 1);
				addNodeToParentOneLevelDown(parent, id, level);
			} else {
				addNodeToParentOneLevelDown(mLastAddedId, id, level);
			}
		}
	}

	/**
	 * Find parent of the node at the level specified.
	 * 
	 * @param node
	 *            node from which we start
	 * @param levelToFind
	 *            level which we are looking for
	 * @return the node found (null if it is topmost node).
	 */
	private T findParentAtLevel(final T node, final int levelToFind) {
		T parent = mManager.getParent(node);
		while (parent != null) {
			if (mManager.getLevel(parent) == levelToFind) {
				break;
			}
			parent = mManager.getParent(parent);
		}
		return parent;
	}

	/**
	 * Adds note to parent at the level specified. But it verifies that the
	 * level is one level down than the parent!
	 * 
	 * @param parent
	 *            parent parent
	 * @param id
	 *            new node id
	 * @param level
	 *            should always be parent's level + 1
	 */
	private void addNodeToParentOneLevelDown(final T parent, final T id,
			final int level) {
		if (parent == null && level != 0) {
			throw new TreeConfigurationException("Trying to add new id " + id
					+ " to top level with level != 0 (" + level + ')');
		}
		if (parent != null && mManager.getLevel(parent) != level - 1) {
			throw new TreeConfigurationException("Trying to add new id " + id
					+ " <" + level + "> to " + parent + " <"
					+ mManager.getLevel(parent)
					+ ">. The difference in levels up is bigger than 1.");
		}
		mManager.addAfterChild(parent, id, null);
		setLastAdded(id, level);
	}

	private void setLastAdded(final T id, final int level) {
		mLastAddedId = id;
		mLastLevel = level;
	}

}
