package pl.polidea.treelistview;

/**
 * Information about the node.
 * 
 * @param <T>
 *            type of the id for the tree
 */
public class TreeNodeInfo<T> {
	private final T mId;
	private final int mLevel;
	private final boolean mWithChildren;
	private final boolean mVisible;
	private final boolean mExpanded;

	/**
	 * Creates the node information.
	 * 
	 * @param id
	 *            id of the node
	 * @param level
	 *            level of the node
	 * @param withChildren
	 *            whether the node has children.
	 * @param visible
	 *            whether the tree node is visible.
	 * @param expanded
	 *            whether the tree node is expanded
	 * 
	 */
	public TreeNodeInfo(final T id, final int level,
			final boolean withChildren, final boolean visible,
			final boolean expanded) {
		super();
		mId = id;
		mLevel = level;
		mWithChildren = withChildren;
		mVisible = visible;
		mExpanded = expanded;
	}

	public T getId() {
		return mId;
	}

	public boolean isWithChildren() {
		return mWithChildren;
	}

	public boolean isVisible() {
		return mVisible;
	}

	public boolean isExpanded() {
		return mExpanded;
	}

	public int getLevel() {
		return mLevel;
	}

	@Override
	public String toString() {
		return "TreeNodeInfo [id=" + mId + ", level=" + mLevel
				+ ", withChildren=" + mWithChildren + ", visible=" + mVisible
				+ ", expanded=" + mExpanded + "]";
	}

}