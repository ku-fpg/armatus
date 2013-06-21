package com.kufpg.armatus.edits;

import java.io.Serializable;

public class EditManager implements Serializable {

	private static final long serialVersionUID = -601384729330799461L;
	private static final int DEFAULT_EDIT_LIMIT = 100;
	private static final int MIN_EDIT_LIMIT = 2;
	private static final int NO_EDITS_INDEX = -99; //Don't make this -1, it'll create bugs

	private EditList mEdits = new EditList();
	private int mCurEditIndex = NO_EDITS_INDEX;
	private int mEditLimit = DEFAULT_EDIT_LIMIT;

	public EditManager() {
		mEdits.ensureCapacity(DEFAULT_EDIT_LIMIT);
	}

	public EditManager(int limit) {
		setLimit(limit);
	}

	public synchronized void applyEdit(Edit edit) {
		if (mCurEditIndex == mEditLimit - 1) {
			mEdits.remove(0);
			mCurEditIndex--;
		}
		if (canRedo()) {
			mEdits.removeRange(mCurEditIndex + 1, mEdits.size());
		}
		mEdits.add(edit);
		if (mCurEditIndex == NO_EDITS_INDEX) {
			mCurEditIndex = 0;
		} else {
			mCurEditIndex++;
		}
		edit.applyEdit();
	}

	public synchronized boolean canRedo() {
		return (mCurEditIndex == NO_EDITS_INDEX && mEdits.size() > 0) ||
				(mCurEditIndex != NO_EDITS_INDEX && mCurEditIndex != mEdits.size() - 1);
	}

	public synchronized boolean canUndo() {
		return mCurEditIndex != NO_EDITS_INDEX;
	}

	public synchronized void discardAllEdits() {
		mEdits.clear();
		mCurEditIndex = NO_EDITS_INDEX;
	}

	public synchronized int getLimit() {
		return mEditLimit;
	}

	public synchronized boolean isSignificant() {
		if (mCurEditIndex != NO_EDITS_INDEX) {
			return mEdits.get(mCurEditIndex).isSignificant();
		} else {
			throw new UnsupportedOperationException("Cannot determine edit significance without at least one edit.");
		}
	}

	public synchronized void redo() {
		if (canRedo()) {
			if (mCurEditIndex == NO_EDITS_INDEX) {
				mCurEditIndex = 0;
			} else {
				mCurEditIndex++;
			}
			mEdits.get(mCurEditIndex).redo();
		} else {
			throw new UnsupportedOperationException("No edits are avaiable to redo.");
		}
	}

	public synchronized void setLimit(int editLimit) {
		if (editLimit >= MIN_EDIT_LIMIT) {
			mEditLimit = editLimit;
			mEdits.ensureCapacity(mEditLimit);
		} else {
			throw new IllegalArgumentException("Edit limit must be at least " + MIN_EDIT_LIMIT + ".");
		}
	}

	public synchronized void undo() {
		if (canUndo()) {
			mEdits.get(mCurEditIndex).undo();
			if (mCurEditIndex == 0) {
				mCurEditIndex = NO_EDITS_INDEX;
			} else {
				mCurEditIndex--;
			}
		} else {
			throw new UnsupportedOperationException("No edits are available to undo.");
		}
	}
}
