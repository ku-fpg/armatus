package com.kufpg.armatus;

import java.util.Iterator;
import java.util.Stack;

import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import com.kufpg.armatus.EditManager.Edit;

/**
 * An <code>EditManager</code> tracks a series of {@link EditManager.Edit Edits}, any of
 * which can be undone or redone if needed. Undoing an <code>Edit</code> pushes it onto
 * the redo stack, and redoing an edit pushes it onto the undo stack. Applying a new <code>
 * Edit</code> pushes it onto the undo stack and clears the redo stack.
 */
public class EditManager implements Iterable<Edit> {
	/**
	 * The maximum number of {@link EditManager.Edit Edits} that this {@link EditManager}
	 * can track. If this limit is reached and another <code>Edit</code> is applied, the
	 * bottom <code>Edit</code> of the undo stack is cleared to make room.
	 */
	private static final int DEFAULT_EDIT_LIMIT = 100;
	
	/**
	 * The minimum amount for the maximum number of {@link EditManager.Edit Edits} that
	 * this {@link EditManager} can track.
	 */
	private static final int MIN_EDIT_LIMIT = 2;

	/**
	 * Calls {@link EditManager.OnEditListener#onEditAction() onEditAction()} whenever an
	 * {@link EditManager.Edit Edit} is invoked in some way.
	 */
	private OnEditListener mListener;
	
	/**
	 * Tracks the {@link EditManager}'s {@link EditManager.Edit Edit} limit.
	 */
	private int mEditLimit = DEFAULT_EDIT_LIMIT;
	
	/**
	 * Tracks {@link EditManager.Edit Edits} that have been previously applied or redone.
	 * Calling {@link #undo EditManager.undo()} will pop an <code>Edit</code> off of this stack
	 * and call its own {@link EditManager.Edit#undo() undo()} method. 
	 */
	private Stack<Edit> mUndoStack = new Stack<Edit>();
	
	/**
	 * Tracks {@link EditManager.Edit Edits} that have been undone. Calling {@link #redo
	 * EditManager.redo()} will pop an <code>Edit</code> off of this stack and call its
	 * own {@link EditManager#redo() redo()} method.
	 */
	private Stack<Edit> mRedoStack = new Stack<Edit>();

	/**
	 * Constructs an empty {@link EditManager} with the default {@link EditManager.Edit Edit}
	 * limit.
	 */
	public EditManager() {
		setLimit(DEFAULT_EDIT_LIMIT);
	}

	/**
	 * Constructs and empty {@link EditManager} with a custom {@link EditManager.Edit Edit}
	 * limit.
	 * @param limit The maximum number of <code>Edits</code> that this <code>EditManager</code>
	 * can track.
	 */
	public EditManager(int limit) {
		setLimit(limit);
	}

	/**
	 * Invokes an {@link EditManager.Edit Edit} for the first time. This method will call the
	 * <code>Edit</code>'s {@link EditManager.Edit#applyEdit() applyEdit()} method, push it
	 * onto the undo stack, and clear the redo stack.
	 * @param edit The <code>Edit</code> being applied.
	 */
	public synchronized void applyEdit(Edit edit) {
		if (mUndoStack.size() == mEditLimit) {
			mUndoStack.remove(0);
		}
		if (canRedo()) {
			mRedoStack.clear();
		}
		mUndoStack.push(edit);
		edit.applyEdit();
		if (mListener != null) {
			mListener.onEditAction();
		}
	}

	/**
	 * Returns whether this {@link EditManager} is capable of redoing an action. By default,
	 * an <code>EditManager</code> can redo if its redo stack is not empty, although subclasses
	 * can override this method to impose additional restrictions.
	 * @return <code>true</code> if {@link EditManager#redo() redo()} can be called successfully.
	 */
	public synchronized boolean canRedo() {
		return !mRedoStack.empty();
	}

	/**
	 * Returns whether this {@link EditManager} is capable of undoing an action. By default,
	 * an <code>EditManager</codE> can undo if its undo stack is not empty, although subclasses
	 * can override this method to impose additional restrictions.
	 * @return <code>true</code> if {@link EditManager#undo() undo()} can be called successfully.
	 */
	public synchronized boolean canUndo() {
		return !mUndoStack.empty();
	}

	/**
	 * Clears the undo and redo stacks.
	 */
	public synchronized void discardAllEdits() {
		mUndoStack.clear();
		mRedoStack.clear();
		if (mListener != null) {
			mListener.onEditAction();
		}
	}

	/**
	 * Returns the maximum {@link EditManager.Edit Edit} limit.
	 * @return the maximum <code>Edit</code> limit.
	 */
	public synchronized int getLimit() {
		return mEditLimit;
	}

	/**
	 * Returns the number of {@link EditManager.Edit Edits} that can be redone.
	 * @return the size of the redo stack.
	 */
	public synchronized int getRemainingRedosCount() {
		return mRedoStack.size();
	}

	/**
	 * Returns the number of {@link EditManager.Edit Edits} that can be undone.
	 * @return the size of the undo stack.
	 */
	public synchronized int getRemainingUndosCount() {
		return mUndoStack.size();
	}

	public synchronized boolean isSignificant() throws UnsupportedOperationException {
		if (canUndo()) {
			return mUndoStack.peek().isSignificant();
		} else {
			throw new UnsupportedOperationException("Cannot determine significance without at least one edit to undo.");
		}
	}

	/**
	 * If possible, this pops an {@link EditManager.Edit Edit} off of the redo stack, calls
	 * its {@link EditManager.Edit#redo() redo()} method, and pushes it onto the undo stack.
	 * @throws UnsupportedOperationException if the {@link EditManager} cannot redo.
	 */
	public synchronized void redo() throws UnsupportedOperationException {
		if (canRedo()) {
			Edit edit = mRedoStack.pop();
			edit.redo();
			mUndoStack.push(edit);
			if (mListener != null) {
				mListener.onEditAction();
			}
		} else {
			throw new UnsupportedOperationException("No edits are avaiable to redo.");
		}
	}

	/**
	 * Sets the maximum number of {@link EditManager.Edit Edits} that this {@link EditManager}
	 * can track.
	 * @param editLimit The desired maximum <code>Edit</code> limit.
	 * @throws IllegalArgumentException if <code>editLimit</code> is less than the minimum
	 * edit limit.
	 */
	public synchronized void setLimit(int editLimit) throws IllegalArgumentException {
		if (editLimit >= MIN_EDIT_LIMIT) {
			mEditLimit = editLimit;
		} else {
			throw new IllegalArgumentException("Edit limit must be at least " + MIN_EDIT_LIMIT + ".");
		}
	}

	/**
	 * Registers a callback to be invoked whenever an {@link EditManager.Edit Edit} is applied,
	 * undone, redone, or cleared.
	 * @param listener The callback that will be run.
	 */
	public synchronized void setOnEditListener(OnEditListener listener) {
		mListener = listener;
	}

	/**
	 * If possible, this pops an {@link EditManager.Edit Edit} off of the undo stack, calls
	 * its {@link EditManager.Edit#undo() undo()} method, and pushes it onto the redo stack.
	 * @throws UnsupportedOperationException if the {@link EditManager} cannot undo.
	 */
	public synchronized void undo() throws UnsupportedOperationException {
		if (canUndo()) {
			Edit edit = mUndoStack.pop();
			edit.undo();
			mRedoStack.push(edit);
			if (mListener != null) {
				mListener.onEditAction();
			}
		} else {
			throw new UnsupportedOperationException("No edits are available to undo.");
		}
	}

	/** An action that can be undone or redone when needed. */
	public interface Edit {
		/**
		 * Called when the {@link Edit} is initially added to an {@link EditManager}. This
		 * is similar to {@link #redo()} except that <code>applyEdit()</code> is only called
		 * once, so it is a good place to do tasks that don't need to be undone.
		 */
		void applyEdit();
		boolean isSignificant();
		/** Executes the {@link Edit}'s action once again after a {@link #undo()}. */
		void redo();
		/**
		 * Reverses the effects of the {@link Edit}'s actions in either {@link #applyEdit()}
		 * or {@link #redo()}.
		 */
		void undo();
	}

	/**
	 * Interface definition for a callback to be invoked whenever an {@link EditManager.Edit
	 * Edit} is applied, undone, redone, or cleared.
	 */
	public interface OnEditListener {
		/**
		 * Called when an {@link EditManager.Edit Edit} is applied, undone, redone, or cleared.
		 */
		void onEditAction();
	}

	@Override
	public Iterator<Edit> iterator() {
		return Iterators.concat(mUndoStack.iterator(), Lists.reverse(mRedoStack).iterator());
	}

}
