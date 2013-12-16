package edu.kufpg.armatus.console;

import java.util.Collection;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.data.CommandResponse;

public abstract class AbsConsoleActivity extends BaseActivity {
	
	public abstract void addCommandHistoryEntry(int fromAst, String command, int toAst);
	public abstract void addCommandResponseEntry(CommandResponse commandResponse);
	public abstract void addErrorResponseEntry(String errorResponse);
	public abstract void addUserInputEntry(String userInput);
	public abstract void appendCommandResponse(CommandResponse commandResponse);
	public abstract void appendErrorResponse(String errorResponse);
	public abstract void appendInputText(String text);
	public abstract void attemptInputCompletion(Collection<String> existingSuggestions);
	public abstract void clearCommandHistory();
	public abstract void clearEntries();
	

}
