package com.kufpg.armatus.edits;

import java.io.Serializable;

public interface Edit extends Serializable {

	void applyEdit();
	boolean isSignificant();
	void redo();
	void undo();

}
