package edu.kufpg.armatus.console;

import java.util.Map;
import java.util.SortedSet;

import com.google.common.base.Function;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.ListMultimap;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import android.content.Intent;
import android.widget.Toast;

/**
 * Contains all {@link CustomCommandInfo}s and {@link Keyword}s that the console uses and allows
 * {@link ConsoleActivity} to execute commands.
 */
public class CustomCommandDispatcher {
	public static final String CLIENT_COMMANDS_TAG = "Client";
	private static final String CLEAR_INFO = "Hides all currently visible console entries. The entries will still be accessible from the command history.";
	private static final String CONNECT_INFO = "Attempts to connect to the HERMIT server. If successful, it will load additional commands.";
	private static final String EXIT_INFO = "Leaves the current console sessions, discarding any unsaved history.";
	private static final String TOAST_INFO = "Displays its arguments as a pop-up on the screen.";
	private static final String TERMINAL_INFO = "Opens Android Terminal Emulator, if installed.";

	private static final CustomCommandInfo CLEAR = new CustomCommandInfo(CLEAR_INFO, "clear", 0, true) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.clear();
		}
	};
	private static final CustomCommandInfo CONNECT = new CustomCommandInfo(CONNECT_INFO, "connect", 1, false) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.getHermitClient().connect("http://" + args[0] + ":3000");
		}
	};
	private static final CustomCommandInfo EXIT = new CustomCommandInfo(EXIT_INFO, "exit", 0) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.exit(false);
		}
	};
	private static final CustomCommandInfo TOAST = new CustomCommandInfo(TOAST_INFO, "toast", 0, true) {
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
	private static final CustomCommandInfo TERMINAL = new CustomCommandInfo(TERMINAL_INFO, "terminal", 0, true){
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

	private static final Map<String, CustomCommandInfo> CUSTOM_COMMAND_NAME_MAP = mapCustomCommands();
	private static final ListMultimap<String, CustomCommandInfo> CUSTOM_COMMAND_TAG_MAP = mapCustomCommandTags();
	private static final SortedSet<String> CUSTOM_COMMAND_NAME_SET = createCustomCommandNameSet();

	private CustomCommandDispatcher() {}
	
	static Map<String, CustomCommandInfo> getCustomCommandNameMap() {
		return CUSTOM_COMMAND_NAME_MAP;
	}
	
	static SortedSet<String> getCustomCommandSet() {
		return CUSTOM_COMMAND_NAME_SET;
	}
	
	static ListMultimap<String, CustomCommandInfo> getCustomCommandTagMap() {
		return CUSTOM_COMMAND_TAG_MAP;
	}
	
	/**
	 * Attempts to run a {@link CustomCommandInfo} on the console.
	 * @param console The {@link ConsoleActivity} on which to run the {@link CustomCommandInfo}.
	 * @param commandName The name of the {@code Command} to run.
	 * @param args The parameters of the {@code Command}.
	 */
	public static void runCustomCommand(ConsoleActivity console, String commandName, String... args) {
		CustomCommandInfo command = CUSTOM_COMMAND_NAME_MAP.get(commandName);
		if (command != null) {
			runCustomCommand(console, command, args);
		}
	}

	/**
	 * Attempts to run a {@link CustomCommandInfo} on the console.
	 * @param console The {@link ConsoleActivity} on which to run the {@link CustomCommandInfo}.
	 * @param commandThe {@code Command} to run.
	 * @param args The parameters of the {@code Command}.
	 */
	private static void runCustomCommand(ConsoleActivity console, CustomCommandInfo command, String... args) {
		if (command.hasLowerArgBound()) {
			if (args.length < command.getArgsCount()) {
				console.appendErrorResponse("ERROR: " + command.getName() +
						" requires at least " + command.getArgsCount() +
						(command.getArgsCount() == 1 ? " argument." :
								" arguments."));
				return;
			}
		} else if (args.length != command.getArgsCount()) {
			console.appendErrorResponse("ERROR: " + command.getName() +
					" requires exactly " + command.getArgsCount() +
					(command.getArgsCount() == 1 ? " argument." :
							" arguments."));
			return;
		}
		command.run(console, args);
	}

	public static CustomCommandInfo getCustomCommand(String commandName) {
		return CUSTOM_COMMAND_NAME_MAP.get(commandName);
	}

	public static boolean isCustomCommand(String commandName) {
		return CUSTOM_COMMAND_NAME_MAP.containsKey(commandName);
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
	
	private static SortedSet<String> createCustomCommandNameSet() {
		return ImmutableSortedSet.copyOf(Collections2.transform(CustomCommandDispatcher.getCustomCommandNameMap().values(),
				new Function<CustomCommandInfo, String>() {
			@Override
			public String apply(CustomCommandInfo info) {
				return info.getName();
			}
		}));
	}

	private static Map<String, CustomCommandInfo> mapCustomCommands() {
		ImmutableMap.Builder<String, CustomCommandInfo> commandBuilder = ImmutableMap.builder();
		return commandBuilder.put(CLEAR.getName(), CLEAR)
				.put(CONNECT.getName(), CONNECT)
				.put(EXIT.getName(), EXIT)
				.put(TERMINAL.getName(), TERMINAL)
				.put(TOAST.getName(), TOAST)
				.build();
	}

	private static ListMultimap<String, CustomCommandInfo> mapCustomCommandTags() {
		ImmutableListMultimap.Builder<String, CustomCommandInfo> tagMapBuilder = ImmutableListMultimap.builder();
		return tagMapBuilder.putAll(CLIENT_COMMANDS_TAG, CUSTOM_COMMAND_NAME_MAP.values()).build();
	}

}
