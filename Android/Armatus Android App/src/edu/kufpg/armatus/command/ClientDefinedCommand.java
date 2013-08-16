package edu.kufpg.armatus.command;

import edu.kufpg.armatus.console.ConsoleActivity;

public class ClientDefinedCommand extends Command {

	public ClientDefinedCommand(String commandName, int argsCount) {
		super(commandName, argsCount);
	}

	public ClientDefinedCommand(String commandName, int argsCount,
			boolean lowerArgBound) {
		super(commandName, argsCount, lowerArgBound);
	}

	@Override
	protected void run(ConsoleActivity console, String... args) {
		console.addCommandEntry(getCommandName());
	}

}
