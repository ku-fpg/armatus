package com.kufpg.armatus;

import java.util.Iterator;
import java.util.List;
import java.util.Stack;

import com.google.common.collect.Iterators;
import com.google.common.collect.Lists;
import com.kufpg.armatus.EditManager.Edit;

public class EditManager implements Iterable<Edit> {
	private static final int DEFAULT_EDIT_LIMIT = 100;
	private static final int MIN_EDIT_LIMIT = 2;

	private OnEditListener mListener;
	private int mEditLimit = DEFAULT_EDIT_LIMIT;
	private Stack<Edit> mUndoStack = new Stack<Edit>();
	private Stack<Edit> mRedoStack = new Stack<Edit>();
	private List<Edit> mRedoStackReverse;

	public EditManager() {
		mRedoStackReverse = Lists.reverse(mRedoStack);
	}

	public EditManager(int limit) {
		this();
		setLimit(limit);
	}

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
			mListener.onEditFinish();
		}
	}

	public synchronized boolean canRedo() {
		return !mRedoStack.empty();
	}

	public synchronized boolean canUndo() {
		return !mUndoStack.empty();
	}

	public synchronized void discardAllEdits() {
		mUndoStack.clear();
		mRedoStack.clear();
		if (mListener != null) {
			mListener.onEditFinish();
		}
	}

	public synchronized int getLimit() {
		return mEditLimit;
	}

	public synchronized int getRemainingRedosCount() {
		return mRedoStack.size();
	}

	public synchronized int getRemainingUndosCount() {
		return mUndoStack.size();
	}

	public synchronized boolean isSignificant() {
		if (canUndo()) {
			return mUndoStack.peek().isSignificant();
		} else {
			throw new UnsupportedOperationException("Cannot determine significance without at least one edit to undo.");
		}
	}

	public synchronized void redo() {
		if (canRedo()) {
			Edit edit = mRedoStack.pop();
			edit.redo();
			mUndoStack.push(edit);
			if (mListener != null) {
				mListener.onEditFinish();
			}
		} else {
			throw new UnsupportedOperationException("No edits are avaiable to redo.");
		}
	}

	public synchronized void setLimit(int editLimit) {
		if (editLimit >= MIN_EDIT_LIMIT) {
			mEditLimit = editLimit;
		} else {
			throw new IllegalArgumentException("Edit limit must be at least " + MIN_EDIT_LIMIT + ".");
		}
	}

	public synchronized void setOnEditListener(OnEditListener listener) {
		mListener = listener;
	}

	public synchronized void undo() {
		if (canUndo()) {
			Edit edit = mUndoStack.pop();
			edit.undo();
			mRedoStack.push(edit);
			if (mListener != null) {
				mListener.onEditFinish();
			}
		} else {
			throw new UnsupportedOperationException("No edits are available to undo.");
		}
	}

	public interface Edit {
		void applyEdit();
		boolean isSignificant();
		void redo();
		void undo();
	}

	public interface OnEditListener {
		void onEditFinish();
	}

	@Override
	public Iterator<Edit> iterator() {
		return Iterators.concat(mUndoStack.iterator(), mRedoStackReverse.iterator());
	}

}
