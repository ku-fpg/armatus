package edu.kufpg.armatus.command;

public abstract class ClientDefinedCommand extends Command {
	public ClientDefinedCommand(String commandName, String groupName,
			int argsCount) {
		super(commandName, groupName, argsCount);
	}

	public ClientDefinedCommand(String commandName, String groupName,
			int argsCount, boolean lowerArgBound) {
		super(commandName, groupName, argsCount, lowerArgBound);
	}
}