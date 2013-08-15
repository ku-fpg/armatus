package edu.kufpg.armatus.command;

public abstract class ServerDefinedCommand extends Command {
	public ServerDefinedCommand(String commandName,	int argsCount) {
		super(commandName, argsCount);
	}

	public ServerDefinedCommand(String commandName,	int argsCount, boolean lowerArgBound) {
		super(commandName, argsCount, lowerArgBound);
	}

}