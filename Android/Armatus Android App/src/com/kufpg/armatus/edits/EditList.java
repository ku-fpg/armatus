package com.kufpg.armatus.edits;

import java.util.ArrayList;

public class EditList extends ArrayList<Edit> {

	private static final long serialVersionUID = 1739846760735027509L;

	//Overridden to increase visibility from protected to public
	@Override
	public void removeRange(int fromIndex, int toIndex) {
		super.removeRange(fromIndex, toIndex);
	}
}
