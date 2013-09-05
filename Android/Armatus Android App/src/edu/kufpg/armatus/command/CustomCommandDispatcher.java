package edu.kufpg.armatus.command;

import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedMap;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import android.content.Intent;
import android.widget.Toast;

/**
 * Contains all {@link CustomCommand}s and {@link Keyword}s that the console uses and allows
 * {@link ConsoleActivity} to execute commands.
 */
public class CustomCommandDispatcher {
	public static final String CLIENT_COMMANDS_TAG = "Client";
	private static final String CLEAR_INFO = "Hides all currently visible console entries. The entries will still be accessible from the command history.";
	private static final String CONNECT_INFO = "Attempts to connect to the HERMIT server. If successful, it will load additional commands.";
	private static final String EXIT_INFO = "Leaves the current console sessions, discarding any unsaved history.";
	private static final String TOAST_INFO = "Displays its arguments as a pop-up on the screen.";
	private static final String TERMINAL_INFO = "Opens Android Terminal Emulator, if installed.";

	private static final CustomCommand CLEAR = new CustomCommand("clear", CLEAR_INFO, 0, true) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.clear();
		}
	};
	private static final CustomCommand CONNECT = new CustomCommand("connect", CONNECT_INFO, 1, false) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.getHermitClient().connect("http://" + args[0] + ":3000");
		}
	};
	private static final CustomCommand EXIT = new CustomCommand("exit", EXIT_INFO, 0) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.exit();
		}
	};
	private static final CustomCommand TOAST = new CustomCommand("toast", TOAST_INFO, 0, true) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			Toast toast = null;
			if (args.length == 0) {
				toast = Toast.makeText(console, "No arguments!", Toast.LENGTH_SHORT);
			} else {
				toast = Toast.makeText(console, varargsToString(args), Toast.LENGTH_SHORT);
			}
			toast.show();
		}
	};
	private static final CustomCommand TERMINAL = new CustomCommand("terminal", TERMINAL_INFO, 0, true){
		@Override
		protected void run(ConsoleActivity console, String... args) {
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
	private static final SortedMap<String, String> CUSTOM_COMMAND_INFO_MAP = mapCustomCommandInfo();

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

	private static Map<String, CustomCommand> mapCustomCommands() {
		ImmutableMap.Builder<String, CustomCommand> commandBuilder = ImmutableMap.builder();
		return commandBuilder.put(CLEAR.getCommandName(), CLEAR)
				.put(CONNECT.getCommandName(), CONNECT)
				.put(EXIT.getCommandName(), EXIT)
				.put(TERMINAL.getCommandName(), TERMINAL)
				.put(TOAST.getCommandName(), TOAST)
				.build();
	}
	
	private static Map<String, String> mapCustomCommandInfo() {
		ImmutableMap.Builder<String, String> commandInfoBuilder = ImmutableMap.builder();
		return commandInfoBuilder.put(CLEAR.getCommandName(), CLEAR.getCommandInfo())
				.put(CONNECT.getCommandName(), CONNECT.getCommandInfo())
				.put(EXIT.getCommandName(), EXIT.getCommandInfo())
				.put(TERMINAL.getCommandName(), TERMINAL.getCommandInfo())
				.put(TOAST.getCommandName(), TOAST.getCommandInfo())
				.build();
	}

}
