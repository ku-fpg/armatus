package com.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;

public class ConsoleEdits {

	public static class ConsoleEntryAdder extends ConsoleEdit {
		private ConsoleEntry mEntry;

		public ConsoleEntryAdder(ConsoleActivity console, String contents) {
			super(console);
			mEntry = new ConsoleEntry(getConsole().getEntryCount(), contents);
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
	
	public static class ConsoleClearer extends ConsoleEdit {
		private List<ConsoleEntry> mOriginalEntries;

		public ConsoleClearer(ConsoleActivity console, List<ConsoleEntry> originalEntries) {
			super(console);
			mOriginalEntries = new ArrayList<ConsoleEntry>(originalEntries);
		}

		@Override
		public void redo() {
			getConsole().clearConsole();
		}

		@Override
		public void undo() {
			getConsole().updateConsoleEntries(mOriginalEntries);
		}
		
	}

}
