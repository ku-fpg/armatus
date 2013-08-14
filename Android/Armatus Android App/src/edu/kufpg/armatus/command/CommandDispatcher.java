package edu.kufpg.armatus.command;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;

import org.json.JSONObject;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedMap;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.ConsoleListView;
import edu.kufpg.armatus.console.PrettyPrinter;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import edu.kufpg.armatus.server.BluetoothUtils;
import edu.kufpg.armatus.server.HermitBluetoothServerRequest;
import edu.kufpg.armatus.server.HermitHttpGetRequest;
import edu.kufpg.armatus.server.HermitWebServerRequest;
import edu.kufpg.armatus.server.InternetUtils;
import edu.kufpg.armatus.util.StringUtils;
import android.content.Intent;
import android.widget.Toast;

/**
 * Contains all {@link Command}s and {@link Keyword}s that the console uses and allows
 * {@link ConsoleActivity} to execute commands.
 */
public class CommandDispatcher {
	public static final String MISCELLANEOUS = "Miscellaneous";

	// All the static final command names for the commands. Each static final runs a function with a
	// string of arguments that will on the console the command they are looking at is in a specific
	// group with the group name. 
	private static final Command BLUETOOTH_TEST = new Command("bluetooth-test", MISCELLANEOUS, 0, true) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			if (BluetoothUtils.isBluetoothEnabled(console)) {
				if (BluetoothUtils.getBluetoothDevice(console) != null) {
					new HermitBluetoothServerRequest(console).execute("From Android with love");
				} else {
					delayCommand(this, false, args);
					BluetoothUtils.findDeviceName(console);
				}
			} else {
				delayCommand(this, false, args);
				BluetoothUtils.enableBluetooth(console);
			}
		}
	};
	private static final Command CLEAR = new Command("clear", MISCELLANEOUS, 0, true) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.clear();
		}
	};
	private static final Command EXIT = new Command("exit", MISCELLANEOUS, 0) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			console.finish();
		}
	};
	private static final Command SERVER_TEST = new Command("server-test", MISCELLANEOUS, 0) {
		@Override
		protected void run(ConsoleActivity console, String... args) {
			try {
				if (InternetUtils.isAirplaneModeOn(console)) {
					console.appendConsoleEntry("Error: Please disable airplane mode before attempting to connect.");
				} else if (!InternetUtils.isWifiConnected(console)) {
					console.appendConsoleEntry("Error: No network connectivity.");
				} else {
					HermitWebServerRequest<String> request = new HermitHttpGetRequest<String>(console) {
						@Override
						protected String onResponse(String response) {
							return response;
						}
						
						@Override
						protected void onPostExecute(String formattedResponse) {
							super.onPostExecute(formattedResponse);
							getActivity().appendConsoleEntry(formattedResponse);
						}
					};
					request.execute("TEST");
				}
			} catch (Exception e) {
				e.printStackTrace();
				return;
			}
		}
	};
	private static final Command TOAST = new Command("toast", MISCELLANEOUS, 0, true) {
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
	private static final Command TERMINAL = new Command("terminal", MISCELLANEOUS, 0, true){
		@Override
		protected void run(ConsoleActivity console, String... args){
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

	/** Maps {@link Command} names to their respective instances. */
	private static final SortedMap<String, Command> COMMAND_MAP = mapCommands();

	//List of Keywords
	private static final Keyword RED = new Keyword("red", "toast", PrettyPrinter.RED);
	private static final Keyword GREEN = new Keyword("green", "toast", PrettyPrinter.GREEN);
	private static final Keyword BLUE = new Keyword("blue", "toast", PrettyPrinter.BLUE);

	/** Maps {@link Keyword} names to their respective instances. */
	private static Map<String, Keyword> KEYWORD_MAP = mapKeywords();

	private static Command mDelayedCommand;
	private static String[] mDelayedCommandArgs;
	private static boolean mIsDelayedCommandSynchronous;

	private CommandDispatcher() {}

	/**
	 * Call this after a {@link Command} has finished so that the command dispatcher will
	 * not execute the delayed {@code Command} later.
	 */
	public static void notifyDelayedCommandFinished() {
		mDelayedCommand = null;
		mDelayedCommandArgs = null;
	}
	
	/**
	 * Runs a {@link Command} if it was delayed for some reason.
	 * @param console The {@link ConsoleActivity} to use.
	 */
	public static void runDelayedCommand(ConsoleActivity console) {
		if (isCommandPending()) {
			mDelayedCommand.run(console, mDelayedCommandArgs);
			if (mIsDelayedCommandSynchronous) {
				notifyDelayedCommandFinished();
			}
		}
	}
	
	/**
	 * Attempts to run a {@link Command} on the console.
	 * @param console The {@link ConsoleActivity} on which to run the {@link Command}.
	 * @param commandName The name of the {@code Command} to run.
	 * @param args The parameters of the {@code Command}.
	 */
	public static void runOnConsole(ConsoleActivity console, String commandName, String... args) {
		Command command = COMMAND_MAP.get(commandName);
		if (command != null) {
			runOnConsole(console, command, args);
		} else {
			console.appendConsoleEntry("Error: " + commandName + " is not a valid command.");
		}
	}

	/**
	 * Attempts to run a {@link Command} on the console.
	 * @param console The {@link ConsoleActivity} on which to run the {@link Command}.
	 * @param commandThe {@code Command} to run.
	 * @param args The parameters of the {@code Command}.
	 */
	private static void runOnConsole(ConsoleActivity console, Command command, String... args) {
		String commandString = command.getCommandName()
				+ StringUtils.NBSP + varargsToString(args);
		console.addConsoleEntry(commandString);
		console.addCommandEntry(command.getCommandName());

		if (command.hasLowerArgBound()) {
			if (args.length < command.getArgsCount()) {
				console.appendConsoleEntry("Error: " + command.getCommandName() +
						" requires at least " + command.getArgsCount() +
						(command.getArgsCount() == 1 ? " argument." :
								" arguments."));
				return;
			}
		} else if (args.length != command.getArgsCount()) {
			console.appendConsoleEntry("Error: " + command.getCommandName() +
					" requires exactly " + command.getArgsCount() +
					(command.getArgsCount() == 1 ? " argument." :
							" arguments."));
			return;
		}
		command.run(console, args);
	}

	/**
	 * Attempts to run the {@link Command} associated with the given {@link
	 * Keyword} name.
	 * @param console The {@link ConsoleActivity} on which to run the {@link Command}.
	 * @param keywordName The name of the {@code Keyword} whose {@code Command}
	 * should be run.
	 * @param arg The parameter of the {@code Command}.
	 */
	public static void runKeywordCommand(ConsoleActivity console, String keywordName, String arg) {
		Keyword keyword = KEYWORD_MAP.get(keywordName);
		if (keyword != null) {
			runOnConsole(console, keyword.getCommand(), arg);
		} else {
			console.appendConsoleEntry("Error: " + keyword + " is not a valid keyword.");
		}
	}

	/**
	 * Returns the {@link Command} instance with the specified name.
	 * @param commandName The name of the {@code Command}.
	 * @return the {@code Command} with the given name, or {@code null} if
	 * {@code commandName} does not correspond to a {@code Command}.
	 */
	public static Command getCommand(String commandName) {
		return COMMAND_MAP.get(commandName);
	}

	/**
	 * Returns a sorted set of all {@link Command} names in {@link CommandDispatcher}.
	 * @return a {@link SortedSet} of all {@code Command} names.
	 */
	static SortedSet<String> getCommandNames() {
		SortedSet<String> commandNames = new TreeSet<String>();
		commandNames.addAll(COMMAND_MAP.keySet());
		return commandNames;
	}

	/**
	 * Return the {@link Keyword} instance with the specified name.
	 * @param keywordName The name of the {@code Keyword}.
	 * @return the {@code Keyword} with the given name, or {@code null} if
	 * {@code keywordName} does not correspond to a {@code Keyword}.
	 */
	public static Keyword getKeyword(String keywordName) {
		return KEYWORD_MAP.get(keywordName);
	}

	/**
	 * Returns whether the specified name is a {@link Command} name.
	 * @param commandName The name to look up.
	 * @return {@code true} if the given name is also a {@code Command} name.
	 */
	public static boolean isCommand(String commandName) {
		return COMMAND_MAP.containsKey(commandName);
	}
	
	/**
	 * Returns whether the command dispatcher is waiting to execute a {@link Command}
	 * that was delayed for some reason.
	 * @return {@code true} if there is a {@code Command} waiting to be executed.
	 */
	public static boolean isCommandPending() {
		return mDelayedCommand != null;
	}

	/**
	 * Returns whether the specified name is a {@link Keyword} name.
	 * @param keywordName The name to look up.
	 * @return {@code true} if the given name is also a <{@code Keyword} name.
	 */
	public static boolean isKeyword(String keywordName) {
		return KEYWORD_MAP.containsKey(keywordName);
	}
	
	/**
	 * "Postpones" execution of a {@link Command} until later. Note that when the {@code
	 * Command} "resumes," it is actually re-executing it. The {@link Command#run(ConsoleActivity, String...)
	 * run(ConsoleActivity, String...)} part of the {@code Command} must behave so that
	 * it appears to be resuming once certain conditions are met.
	 * @param command The {@code Command} to postpone.
	 * @param synchronous {@code true} if the delayed {@code Command} reference can be reset immediately
	 * after it ends, {@code false} if the {@code Command} operates asynchronously and must wait.
	 * @param args The arguments to pass to the {@code Command}.
	 */
	private static void delayCommand(Command command, boolean synchronous, String... args) {
		mDelayedCommand = command;
		mDelayedCommandArgs = args;
		mIsDelayedCommandSynchronous = synchronous;
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

	/**
	 * Initializes {@link #COMMAND_MAP} by mapping {@link Command} names to their
	 * respective instances.
	 * @return a {@link SortedMap} of {@code Command} names to their instances.
	 */
	private static SortedMap<String, Command> mapCommands() {
		ImmutableSortedMap.Builder<String, Command> commandBuilder = ImmutableSortedMap.naturalOrder();
		Field[] fields = CommandDispatcher.class.getDeclaredFields();
		for (Field f : fields) {
			try {
				if (f.getType().equals(Command.class)) {
					Command command = (Command) f.get(Command.class);
					commandBuilder.put(command.getCommandName(), command);
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return commandBuilder.build();
	}

	/**
	 * Initializes {@link #KEYWORD_MAP} by mapping {@link Keyword} names to their
	 * respective instances.
	 * @return a {@link Map} of {@code Keyword} names to their instances.
	 */
	private static Map<String, Keyword> mapKeywords() {
		return ImmutableMap.of("red", RED, "green", GREEN, "blue", BLUE);
	}

}
