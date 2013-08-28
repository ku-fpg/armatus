package edu.kufpg.armatus.command;

import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;

import com.google.common.collect.ImmutableSortedMap;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import edu.kufpg.armatus.util.StringUtils;
import android.content.Intent;
import android.widget.Toast;

/**
 * Contains all {@link CustomCommand}s and {@link Keyword}s that the console uses and allows
 * {@link ConsoleActivity} to execute commands.
 */
public class CustomCommandDispatcher {
	public static final String CLIENT_COMMANDS_TAG = "Client";

	private static final CustomCommand CLEAR = new CustomCommand("clear", 0, true) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.clear();
			super.run(console, args);
		}
	};
	private static final CustomCommand EXIT = new CustomCommand("exit", 0) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.exit();
		}
	};
	private static final CustomCommand TOAST = new CustomCommand("toast", 0, true) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			Toast toast = null;
			if (args.length == 0) {
				toast = Toast.makeText(console, "No arguments!", Toast.LENGTH_SHORT);
			} else {
				toast = Toast.makeText(console, varargsToString(args), Toast.LENGTH_SHORT);
			}
			toast.show();
			super.run(console, args);
		}
	};
	private static final CustomCommand TERMINAL = new CustomCommand("terminal", 0, true){
		@Override
		protected void run(ConsoleActivity console, String... args) {
			super.run(console, args);
			String packageName = "jackpal.androidterm";
			boolean installed = BaseActivity.appInstalledOrNot(console, packageName);  
			if (installed) {
				Intent i = new Intent("jackpal.androidterm.RUN_SCRIPT");
				i.addCategory(Intent.CATEGORY_DEFAULT);
				i.putExtra("jackpal.androidterm.iInitialCommand", varargsToString(args));
				console.startActivity(i);
			} else {
				TerminalNotInstalledDialog tnid = new TerminalNotInstalledDialog();
				tnid.show(console.getFragmentManager(), "tnid");
			}
		}
	};

	private static final SortedMap<String, CustomCommand> CUSTOM_COMMAND_MAP = mapCustomCommands();

	private CustomCommandDispatcher() {}

	/**
	 * Attempts to run a {@link CustomCommand} on the console.
	 * @param console The {@link ConsoleActivity} on which to run the {@link CustomCommand}.
	 * @param commandName The name of the {@code Command} to run.
	 * @param args The parameters of the {@code Command}.
	 */
	public static void runCustomCommand(ConsoleActivity console, String commandName, String... args) {
		CustomCommand command = CUSTOM_COMMAND_MAP.get(commandName);
		if (command != null) {
			runCustomCommand(console, command, args);
		}
	}

	/**
	 * Attempts to run a {@link CustomCommand} on the console.
	 * @param console The {@link ConsoleActivity} on which to run the {@link CustomCommand}.
	 * @param commandThe {@code Command} to run.
	 * @param args The parameters of the {@code Command}.
	 */
	private static void runCustomCommand(ConsoleActivity console, CustomCommand command, String... args) {
		String commandString = command.getCommandName()
				+ StringUtils.NBSP + varargsToString(args);
		console.addConsoleUserInputEntry(commandString);

		if (command.hasLowerArgBound()) {
			if (args.length < command.getArgsCount()) {
				console.appendErrorResponse("ERROR: " + command.getCommandName() +
						" requires at least " + command.getArgsCount() +
						(command.getArgsCount() == 1 ? " argument." :
								" arguments."));
				return;
			}
		} else if (args.length != command.getArgsCount()) {
			console.appendErrorResponse("ERROR: " + command.getCommandName() +
					" requires exactly " + command.getArgsCount() +
					(command.getArgsCount() == 1 ? " argument." :
							" arguments."));
			return;
		}
		command.run(console, args);
	}

	public static CustomCommand getCustomCommand(String commandName) {
		return CUSTOM_COMMAND_MAP.get(commandName);
	}

	public static SortedSet<String> getCustomCommandNames() {
		SortedSet<String> commandNames = new TreeSet<String>();
		commandNames.addAll(CUSTOM_COMMAND_MAP.keySet());
		return commandNames;
	}
	
	public static boolean isCustomCommand(String commandName) {
		return CUSTOM_COMMAND_MAP.containsKey(commandName);
	}

	/**
	 * Combines several strings into a single string, which each original string separated
	 * by a space.
	 * @param varargs The strings to combine into one.
	 * @return A new string consisting of each input string separated by a space.
	 */
	private static String varargsToString(String... varargs) {
		StringBuilder builder = new StringBuilder();
		for(String string : varargs) {
			builder.append(string).append(" ");
		}
		return builder.toString().trim();
	}

	private static SortedMap<String, CustomCommand> mapCustomCommands() {
		ImmutableSortedMap.Builder<String, CustomCommand> commandBuilder = ImmutableSortedMap.naturalOrder();
		return commandBuilder.put(CLEAR.getCommandName(), CLEAR)
				.put(EXIT.getCommandName(), EXIT)
				.put(TERMINAL.getCommandName(), TERMINAL)
				.put(TOAST.getCommandName(), TOAST)
				.build();
	}

}
