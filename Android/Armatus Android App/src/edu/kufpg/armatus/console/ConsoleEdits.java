package edu.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;

/**
 * Class containing the various {@link ConsoleEdit}s that {@link ConsoleActivity} can use.
 */
public class ConsoleEdits {

	/**
	 * {@link ConsoleEdit} that adds a {@link ConsoleEntry} to the console.
	 */
	public static class AddEntry extends ConsoleEdit {
		/** The {@link ConsoleEntry} to be added. */
		private ConsoleEntry mEntry;

		/**
		 * Constructs a new instance, where the {@link ConsoleEntry} will contain the
		 * specified contents.
		 * @param console reference to the current console.
		 * @param contents what the {@code ConsoleEntry} to be added will contain.
		 */
		public AddEntry(ConsoleActivity console, CharSequence contents) {
			super(console);
			mEntry = new ConsoleEntry(getConsole().getInputNum(), contents);
		}

		@Override
		public void redo() {
			getConsole().addConsoleEntry(mEntry);
		}

		@Override
		public void undo() {
			getConsole().removeConsoleEntry();
		}
	}
	
	/**
	 * {@link ConsoleEdit} that clears the console of all entries.
	 */
	public static class Clear extends ConsoleEdit {
		/** Contains the {@link ConsoleEntry ConsoleEntries} that are to be cleared. */
		private List<ConsoleEntry> mOriginalEntries;
		/** The {@link ConsoleEntry} number used for the {@code clear} command. */
		private int mInputNum;

		/**
		 * Constructs a new instance with references to the {@link ConsoleEntry} number
		 * used for the {@code clear} commands as well as the entries that are to be
		 * cleared.
		 * @param console reference to the current console.
		 * @param inputNum the entry number used for the {@code clear} command.
		 * @param originalEntries the entries that are to be cleared.
		 */
		public Clear(ConsoleActivity console, int inputNum, List<ConsoleEntry> originalEntries) {
			super(console);
			mInputNum = inputNum;
			mOriginalEntries = new ArrayList<ConsoleEntry>(originalEntries);
		}

		@Override
		public void redo() {
			getConsole().clearConsole();
		}

		@Override
		public void undo() {
			getConsole().updateConsoleEntries(mInputNum, mOriginalEntries);
		}
		
	}

}
