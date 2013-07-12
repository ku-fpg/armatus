package com.kufpg.armatus.console;

import com.kufpg.armatus.EditManager.Edit;

public class ConsoleEdit implements Edit {
	
	private ConsoleActivity mConsole;
	
	public ConsoleEdit(ConsoleActivity console) {
		mConsole = console;
	}

	@Override
	public void applyEdit() {
		redo();
	}
	
	void attachConsole(ConsoleActivity console) {
		mConsole = console;
	}
	
	public ConsoleActivity getConsole() {
		return mConsole;
	}

	@Override
	public boolean isSignificant() { return true; }

	@Override
	public void redo() {}

	@Override
	public void undo() {}
	
}
