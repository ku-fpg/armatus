package com.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;

public class ConsoleEdits {

	public static class AddEntry extends ConsoleEdit {
		private ConsoleEntry mEntry;

		public AddEntry(ConsoleActivity console, String contents) {
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
	
	public static class Clear extends ConsoleEdit {
		private List<ConsoleEntry> mOriginalEntries;
		private int mInputNum;

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
