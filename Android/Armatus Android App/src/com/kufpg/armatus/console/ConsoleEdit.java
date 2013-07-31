package com.kufpg.armatus.console;

import com.kufpg.armatus.EditManager.Edit;

/**
 * An {@link Edit} that interacts with a {@link ConsoleActivity}.
 */
public class ConsoleEdit implements Edit {
	/** Reference to the current console. */
	private ConsoleActivity mConsole;
	
	public ConsoleEdit(ConsoleActivity console) {
		mConsole = console;
	}

	@Override
	public void applyEdit() {
		redo();
	}
	
	/**
	 * Restores the reference to the current console, which can be destroyed after
	 * device standby or rotation.
	 * @param console The {@link ConsoleActivity} to reconnect to.
	 */
	void attachConsole(ConsoleActivity console) {
		mConsole = console;
	}
	
	/**
	 * Returns a reference to the current console.
	 * @return the referenced {@link ConsoleActivity}.
	 */
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
