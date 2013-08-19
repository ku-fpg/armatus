package edu.kufpg.armatus.networking;

import edu.kufpg.armatus.console.ConsoleActivity;

public abstract class AbsHermitClient implements HermitClient {
	private ConsoleActivity mConsole;
	
	public AbsHermitClient(ConsoleActivity console) {
		mConsole = console;
	}
	
	public ConsoleActivity getConsole() {
		return mConsole;
	}

	@Override
	public abstract void init();

	@Override
	public abstract void doCommand();

	@Override
	public abstract void getCommands();

	@Override
	public abstract void reset();

}
