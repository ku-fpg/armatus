package com.kufpg.armatus.edits;

public interface Edit {

	void applyEdit();
	boolean isSignificant();
	void redo();
	void undo();

}
