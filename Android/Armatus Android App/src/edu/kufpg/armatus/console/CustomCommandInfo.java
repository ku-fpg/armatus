package edu.kufpg.armatus.console;

import java.util.Arrays;

import edu.kufpg.armatus.data.CommandInfo;

public abstract class CustomCommandInfo extends CommandInfo {
	/** The number of arguments that this {@link CustomCommandInfo} takes. If {@link
	 * #mLowerArgBound} is {@code true}, {@code mArgsCount} specifies the
	 * <em>minimum</em> number of arguments that this {@code Command} can take. */
	private int mArgsCount;

	/** {@code true } if {@link #mArgsCount} is a minimum amount, {@code false} if
	 * {@code mArgsCount} is a precise quantity. */
	private boolean mLowerArgBound = false;

	
	public CustomCommandInfo(String help, String name, int argsCount) {
		super(help, name, Arrays.asList(CustomCommandDispatcher.CLIENT_COMMANDS_TAG));
		mArgsCount = argsCount;
	}

	public CustomCommandInfo(String help, String name, int argsCount, boolean lowerArgBound) {
		this(help, name, argsCount);
		mLowerArgBound = lowerArgBound;
	}

	/**
	 * Return the (possibly minimum) number of arguments that this {@link CustomCommandInfo}
	 * must take.
	 * @return the number of arguments that the {@code Command} takes.
	 */
	public int getArgsCount() {
		return mArgsCount;
	}

	/**
	 * Returns whether the {@link CustomCommandInfo} accepts a minimum number of arguments.
	 * @return if the {@code Command} has a lower argument bound.
	 */
	public boolean hasLowerArgBound() {
		return mLowerArgBound;
	}

	/**
	 * The instructions that are ran when this {@link CustomCommandInfo} is run on the console.
	 * @param console The {@link ConsoleActivity} on which this {@code Command} will be run.
	 * @param args Parameters that the {@code Command} uses.
	 */
	protected abstract void run(ConsoleActivity console, String... args);
}
