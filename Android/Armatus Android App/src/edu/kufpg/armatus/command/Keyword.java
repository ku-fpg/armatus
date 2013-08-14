package edu.kufpg.armatus.command;

/**
 * As opposed to a {@link Command}, a {@code Keyword} is a word that {@link 
 * PrettyPrinter} singles out as important (by coloring it). When a {@code Keyword}
 * is accessed by long-clicking a {@link ConsoleListView} entry and selecting it via
 * the context menu, a corresponding {@code Command} is run.
 */
public class Keyword {
	/** The name of the {@link Keyword}. */
	private String mKeywordName;

	/** The {@link Command} associated with this {@link Keyword} when chosen in a
	 * context menu. */
	private Command mCommand;

	/** How {@link PrettyPrinter} colors this {@link Keyword}. Represented as a
	 * hexadecimal string. */
	private String mColor;

	/**
	 * Constructs a new instance.
	 * @param keywordName The name of the {@link Keyword}.
	 * @param commandName The name of the {@link Command} associated with this {@code
	 * Keyword} when chosen in a context menu.
	 * @param color The hexadecimal string representation of the color that {@link
	 * PrettyPrinter} uses to color this {@code Keyword}.
	 */
	public Keyword(String keywordName, String commandName, String color) {
		mKeywordName = keywordName;
		if (isCommand(commandName)) {
			mCommand = COMMAND_MAP.get(commandName);
		} else {
			mCommand = CommandDispatcher.getCommand("toast");
		}
		mColor = color;
	}

	/**
	 * Returns the name of this {@link Keyword}.
	 * @return the {@code Keyword} name.
	 */
	public String getKeywordName() {
		return mKeywordName;
	}

	/**
	 * Returns the {@link Command} associated with this {@link Keyword} when
	 * chosen in a context menu.
	 * @return the {@code Command} associated with this {@code Keyword}.
	 */
	public Command getCommand() {
		return mCommand;
	}

	/**
	 * Returns the hexadecimal string representation of the color that {@link
	 * PrettyPrinter} uses to color this {@link Keyword}.
	 * @return {@code PrettyPrinter}'s color for this {@code Keyword}.
	 */
	public String getColor() {
		return mColor;
	}
}