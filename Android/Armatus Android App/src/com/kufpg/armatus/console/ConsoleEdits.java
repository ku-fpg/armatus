package com.kufpg.armatus.console;

public class ConsoleEdits {

	public static class ConsoleEntryAdder extends ConsoleEdit {
		private ConsoleEntry mEntry;

		public ConsoleEntryAdder(ConsoleActivity console, String contents) {
			super(console);
			mEntry = new ConsoleEntry(getConsole().getEntryCount(), contents);
		}

		@Override
		public void applyEdit() {
			getConsole().addConsoleEntry(mEntry);
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

}
