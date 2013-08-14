package edu.kufpg.armatus.command;

public abstract class ServerDefinedCommand extends Command {
	public ServerDefinedCommand(String commandName, String groupName,
			int argsCount) {
		super(commandName, groupName, argsCount);
	}

	public ServerDefinedCommand(String commandName, String groupName,
			int argsCount, boolean lowerArgBound) {
		super(commandName, groupName, argsCount, lowerArgBound);
	}

}