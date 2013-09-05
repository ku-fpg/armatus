package edu.kufpg.armatus.command;

import edu.kufpg.armatus.console.ConsoleActivity;

/**
 * Provides instructions (via the {@link #run(ConsoleActivity, String...)} method) for the console to
 * execute when the user enters a command. A {@code Command} can have any number
 * of arguments and may accept at least a certain number of arguments if it is
 * initialized with a lower argument bound.
 */
public abstract class CustomCommand {
	/** The real {@link CustomCommand} name. */
	private String mCommandName;
	
	private String mCommandInfo;

	/** The number of arguments that this {@link CustomCommand} takes. If {@link
	 * #mLowerArgBound} is {@code true}, {@code mArgsCount} specifies the
	 * <em>minimum</em> number of arguments that this {@code Command} can take. */
	private int mArgsCount;

	/** {@code true } if {@link #mArgsCount} is a minimum amount, {@code false} if
	 * {@code mArgsCount} is a precise quantity. */
	private boolean mLowerArgBound = false;

	/**
	 * Constructs a new instance.
	 * @param commandName The name of the {@link CustomCommand}.
	 * @param argsCount The number of arguments that this {@code Command} must take.
	 */
	public CustomCommand(String commandName, String commandInfo, int argsCount) {
		mCommandName = commandName;
		mCommandInfo = commandInfo;
		mArgsCount = argsCount;
	}

	/**
	 * Constructs a new instance, specifying if the {@link CustomCommand} has a lower
	 * argument bound.
	 * @param commandName The name of the {@code Command}.
	 * @param argsCount The number of arguments that this {@code Command} must
	 * take. If {@code lowerArgBound} is {@code true}, this is a minimum amount.
	 * @param lowerArgBound {@code true} if {@code argsCount} is a minimum
	 * amount, {@code false} if {@code argsCount} is a precise quantity.
	 */
	public CustomCommand(String commandName, String commandInfo, int argsCount, boolean lowerArgBound) {
		this(commandName, commandInfo, argsCount);
		mLowerArgBound = lowerArgBound;
	}
	
	public String getCommandInfo() {
		return mCommandInfo;
	}

	/**
	 * Returns the true name of the {@link CustomCommand}.
	 * @return the real (not aliased) name of the {@code Command}.
	 */
	public String getCommandName() {
		return mCommandName;
	}

	/**
	 * Return the (possibly minimum) number of arguments that this {@link CustomCommand}
	 * must take.
	 * @return the number of arguments that the {@code Command} takes.
	 */
	public int getArgsCount() {
		return mArgsCount;
	}

	/**
	 * Returns whether the {@link CustomCommand} accepts a minimum number of arguments.
	 * @return if the {@code Command} has a lower argument bound.
	 */
	public boolean hasLowerArgBound() {
		return mLowerArgBound;
	}

	/**
	 * The instructions that are ran when this {@link CustomCommand} is run on the console.
	 * @param console The {@link ConsoleActivity} on which this {@code Command} will be run.
	 * @param args Parameters that the {@code Command} uses.
	 */
	protected abstract void run(ConsoleActivity console, String... args);
}
