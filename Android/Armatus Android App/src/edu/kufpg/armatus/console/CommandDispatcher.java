package edu.kufpg.armatus.console;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;

import org.json.JSONObject;

import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedMap;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import edu.kufpg.armatus.server.HermitServer;
import edu.kufpg.armatus.util.NetworkUtils;
import edu.kufpg.armatus.util.StringUtils;

import android.content.Intent;
import android.widget.Toast;

/**
 * Contains all {@link Command}s and {@link Keyword}s that the console uses and allows
 * {@link ConsoleActivity} to execute commands.
 */
@SuppressWarnings("unused")
public class CommandDispatcher {
	public static final String ALPHA_CONVERSATION = "Alpha Conversation";
	public static final String FIX_POINT = "Fix Point";
	public static final String LOCAL = "Local";
	public static final String NEW_DEBUG_NAV_GHC = "New/Debug/Nav/GHC";
	public static final String UNFOLD_FOLD_INLINE = "Unfold/Fold/Inline";
	public static final String MISCELLANEOUS = "Miscellaneous";

	/** Reference to the current console */
	private static ConsoleActivity mConsole;

	//List of Commands
	/**
	 * All the static final command names for the commands. Each static final runs a function with a string of arguments that will on the console the command they are looking at is in a specific group with the group name. 
	 */
	private static final Command ALPHA = new Command("alpha", ALPHA_CONVERSATION, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ALPHA_ALT = new Command("alpha-alt", ALPHA_CONVERSATION, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ALPHA_CASE = new Command("alpha-case", ALPHA_CONVERSATION, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ALPHA_CASE_BINDER = new Command("alpha-case-binder", ALPHA_CONVERSATION, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ALPHA_LAM = new Command("alpha-lam", ALPHA_CONVERSATION, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ALPHA_LET = new Command("alpha-let", ALPHA_CONVERSATION, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ALPHA_TOP = new Command("alpha-top", ALPHA_CONVERSATION, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command UNDERSHADOW = new Command("undershadow", ALPHA_CONVERSATION, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static final Command FIX_COMPUTATION = new Command("fix-computation", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command FIX_INTRO = new Command("fix-intro", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ROLLING_RULE = new Command("rolling-rule", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command WW_ASSUMPTION_A = new Command("ww-assumption-a", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command wwAssumptionB = new Command("ww-assumption-b", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command WW_ASSUMPTION_C = new Command("ww-assumption-c", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command WW_FACTORISATION = new Command("ww-factorisation", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command WW_FUSION = new Command("ww-fusion", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command WW_SPLIT = new Command("ww-split", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command WW_SPLIT_PARAM = new Command("ww-split-param", FIX_POINT, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static final Command ABSTRACT = new Command("abstract", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command BETA_EXPAND = new Command("beta-expand", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command BETA_REDUCE = new Command("beta-reduce", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command BETA_REDUCE_PLUS = new Command("beta-reduce-plus", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ETA_EXPAND = new Command("eta-expand", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ETA_REDUCE = new Command("eta-reduce", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command FLATTEN_MODULE = new Command("flatten-module", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command FLATTEN_PROGRAM = new Command("flatten-program", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command NONREC_TO_REC = new Command("nonrec-to-rec", LOCAL, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static final Command ADD_RULE = new Command("add-rule", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command ANY_CALL = new Command("any-call", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command APPLY_RULE = new Command("apply-rule", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command APPLY_RULES = new Command("apply-rules", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command BINDING_GROUP_OF = new Command("binding-group-of", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command CAST_ELIMINATE = new Command("cast-eliminate", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command CLEANUP_UNFOLD = new Command("cleanup-unfold", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command COMPARE_VALUES = new Command("compare-values", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command CONSIDER = new Command("consider", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command CONSIDER_CONSTRUCT = new Command("consider-construct", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command CONSIDER_NAME = new Command("consider-name", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command DEBUG_UNFOLD = new Command("debug-unfold", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command DESHADOW_PROGRAM = new Command("deshadow-program", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command FREE_IDS = new Command("free-ids", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command INFO = new Command("info", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command INLINE_ALL = new Command("inline-all", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command LET_SUB = new Command("let-sub", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command LET_TUPLE = new Command("let-tuple", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command OBSERVE = new Command("observe", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command OBSERVE_FAIL = new Command("observe-fail", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command OCCURE_ANALYSIS = new Command("occure-analysis", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command PUSH = new Command("push", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command RESUME = new Command("resume", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command REWRITES = new Command(">>>", "rewrites", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command REWRITES_ONE_FAIL = new Command(">+>", "rewrites-one-fail", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command RHS_OF = new Command("rhs-of", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command SAFE_LET_SUB = new Command("safe-let-sub", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command SIMPLIFY = new Command("simplify", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command STATIC_ARGUMENT = new Command("static-argument", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command TEST = new Command("test", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command TRACE = new Command("trace", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command TRANSLATE_REWRITE = new Command("<+", "translate-rewrite", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command UNFOLD_RULE = new Command("unfold-rule", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command UNSAFE_REPLACE = new Command("unsafe-replace", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command VARIABLE = new Command("variable", NEW_DEBUG_NAV_GHC, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static final Command FOLD = new Command("fold", UNFOLD_FOLD_INLINE, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command FOLD_REMEMBERED = new Command("fold-remembered", UNFOLD_FOLD_INLINE, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command INLINE = new Command("inline", UNFOLD_FOLD_INLINE, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command INLINE_CASE_BINDER = new Command("inline-case-binder", UNFOLD_FOLD_INLINE, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command INLINE_NAME = new Command("inline-name", UNFOLD_FOLD_INLINE, 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command INLINE_SCRUTINEE = new Command("inline-scrutinee", UNFOLD_FOLD_INLINE, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command REMEMBER = new Command("remember", UNFOLD_FOLD_INLINE, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static final Command UNFOLD = new Command("unfold", UNFOLD_FOLD_INLINE, 0) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static final Command CLEAR = new Command("clear", MISCELLANEOUS, 0, true) {
		@Override
		protected void run(String... args) {
			mConsole.clear();
		}
	};
	private static final Command EXIT = new Command("exit", MISCELLANEOUS, 0) {
		@Override
		protected void run(String... args) {
			mConsole.finish();
		}
	};
	private static final Command SERVER_TEST = new Command("server-test", MISCELLANEOUS, 0) {
		@Override
		protected void run(String... args) {
			try {
				if (NetworkUtils.isAirplaneModeOn(mConsole)) {
					mConsole.appendConsoleEntry("Error: Please disable airplane mode before attempting to connect.");
				} else if (!NetworkUtils.isWifiConnected(mConsole)) {
					mConsole.appendConsoleEntry("Error: No network connectivity.");
				} else {
					String jstr = "{command:server-test},{args:" + varargsToString(args) + "}";
					HermitServer request = new HermitServer(mConsole);
					request.execute(new JSONObject(jstr));
				}
			} catch (Exception e) {
				e.printStackTrace();
				return;
			}
		}
	};
	private static final Command TOAST = new Command("toast", MISCELLANEOUS, 0, true) {
		@Override
		protected void run(String... args) {
			Toast toast = null;
			if (args.length == 0) {
				toast = Toast.makeText(mConsole, "No arguments!", Toast.LENGTH_SHORT);
			} else {
				toast = Toast.makeText(mConsole,
						varargsToString(args), Toast.LENGTH_SHORT);
			}
			toast.show();
		}
	};
	private static final Command TERMINAL = new Command("terminal", MISCELLANEOUS, 0, true){
		@Override
		protected void run(String... args){
			String packageName = "jackpal.androidterm";
			boolean installed = BaseActivity.appInstalledOrNot(mConsole, packageName);  
			if (installed) {
				Intent i = new Intent("jackpal.androidterm.RUN_SCRIPT");
				i.addCategory(Intent.CATEGORY_DEFAULT);
				i.putExtra("jackpal.androidterm.iInitialCommand", varargsToString(args));
				mConsole.startActivity(i);
			} else {
				TerminalNotInstalledDialog tnid = new TerminalNotInstalledDialog();
				tnid.show(mConsole.getFragmentManager(), "tnid");
			}
		}
	};

	/** Maps {@link Command} names to their respective instances. */
	private static final SortedMap<String, Command> COMMAND_MAP = mapCommands();

	/**
	 * Maps {@link Command} group names to their group colors (in hexadecimal string
	 * form) as displayed in the {@link ConsoleActivity}'s {@link android.widget.ExpandableListView
	 * ExpandableListView} of {@link edu.kufpg.armatus.drag.DragIcon DragIcons}.
	 * */
	private static final Map<String, String> GROUP_COLOR_MAP = mapGroupColors();

	/** Maps any {@link Command} aliases to their true names. */
	private static final Map<String, String> ALIASED_COMMAND_MAP = mapAliasedCommands();

	//List of Keywords
	private static final Keyword RED = new Keyword("red", "toast", PrettyPrinter.RED);
	private static final Keyword GREEN = new Keyword("green", "toast", PrettyPrinter.GREEN);
	private static final Keyword BLUE = new Keyword("blue", "toast", PrettyPrinter.BLUE);

	/** Maps{@link Keyword} names to their respective instances. */
	private static Map<String, Keyword> KEYWORD_MAP = mapKeywords();

	/**
	 * Constructs a new instance.
	 * @param console reference to the current console.
	 */
	public CommandDispatcher(ConsoleActivity console) {
		mConsole = console;
	}

	/**
	 * Attempts to run a {@link Command} on the console.
	 * @param commandName The name of the {@code Command} to run.
	 * @param args The parameters of the {@code Command}.
	 */
	public void runOnConsole(String commandName, String... args) {
		Command command = COMMAND_MAP.get(commandName);
		if (command != null) {
			runOnConsole(command, args);
		} else {
			mConsole.appendConsoleEntry("Error: " + commandName + " is not a valid command.");
		}
	}

	/**
	 * Attempts to run a {@link Command} on the console.
	 * @param commandThe {@code Command} to run.
	 * @param args The parameters of the {@code Command}.
	 */
	private void runOnConsole(Command command, String... args) {
		String commandString = command.getCommandName()
				+ StringUtils.NBSP + varargsToString(args);
		mConsole.addConsoleEntry(commandString);
		mConsole.addCommandEntry(command.getCommandName());

		if (command.hasLowerArgBound()) {
			if (args.length < command.getArgsCount()) {
				mConsole.appendConsoleEntry("Error: " + command.getCommandName() +
						" requires at least " + command.getArgsCount() +
						(command.getArgsCount() == 1 ? " argument." :
								" arguments."));
				return;
			}
		} else if (args.length != command.getArgsCount()) {
			mConsole.appendConsoleEntry("Error: " + command.getCommandName() +
					" requires exactly " + command.getArgsCount() +
					(command.getArgsCount() == 1 ? " argument." :
							" arguments."));
			return;
		}
		command.run(args);
	}

	/**
	 * Attempts to run the {@link Command} associated with the given {@link
	 * Keyword} name.
	 * @param keywordName The name of the {@code Keyword} whose {@code Command}
	 * should be run.
	 * @param arg The parameter of the {@code Command}.
	 */
	public void runKeywordCommand(String keywordName, String arg) {
		Keyword keyword = KEYWORD_MAP.get(keywordName);
		if (keyword != null) {
			runOnConsole(keyword.getCommand(), arg);
		} else {
			mConsole.appendConsoleEntry("Error: " + keyword + " is not a valid keyword.");
		}
	}

	/**
	 * Returns whether the specified name is a {@link Command} alias.
	 * @param commandName The name to look up.
	 * @return {@code true} if the name is a {@code Command} alias.
	 */
	public static boolean isAlias(String commandName) {
		return ALIASED_COMMAND_MAP.containsKey(commandName);
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
	 * Returns whether the specified name is a {@link Keyword} name.
	 * @param keywordName The name to look up.
	 * @return {@code true} if the given name is also a <{@code Keyword} name.
	 */
	public static boolean isKeyword(String keywordName) {
		return KEYWORD_MAP.containsKey(keywordName);
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
	 * Returns the hexadecimal string representation of the color associated with the
	 * specified {@link Command} group name.
	 * @param groupName The {@code Command} group name.
	 * @return the color of the group, or {@code null} if {@code groupName}
	 * does not correspond to a group.
	 */
	public static String getGroupColor(String groupName) {
		return GROUP_COLOR_MAP.get(groupName);
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
	 * Returns the real {@link Command} name associated with the specified alias.
	 * @param alias The name of the alias for which the real name should be returned.
	 * @return The true {@code Command} name, or {@code null} if {@code alias} does
	 * not correspond to a {@code Command} name.
	 */
	public static String unaliasCommand(String alias) {
		return ALIASED_COMMAND_MAP.get(alias);
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
	 * Initializes {@link #ALIASED_COMMAND_MAP} by mapping any {@link Command} aliases
	 * to their true command names.
	 * @return a {@link Map} of command alias names to their real names.
	 */
	private static Map<String, String> mapAliasedCommands() {
		ImmutableMap.Builder<String, String> aliasBuilder = ImmutableMap.builder();
		Field[] fields = CommandDispatcher.class.getDeclaredFields();
		for (Field f : fields) {
			try {
				if (f.getType().equals(Command.class)) {
					Command command = (Command) f.get(Command.class);
					if (command.getCommandAlias() != null) {
						aliasBuilder.put(command.getCommandAlias(), command.getCommandName());
					}
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return aliasBuilder.build();
	}

	/**
	 * Initializes {@link #GROUP_COLOR_MAP} by mapping group names to their group colors
	 * (in hexadecimal string form) as displayed in the {@link ConsoleActivity}'s {@link
	 * android.widget.ExpandableListView ExpandableListView} of {@link
	 * edu.kufpg.armatus.drag.DragIcon DragIcons}.
	 * @return a {@link Map} of group names to their group colors.
	 */
	private static Map<String, String> mapGroupColors() {
		ImmutableMap.Builder<String, String> groupColorBuilder = ImmutableMap.builder();
		return groupColorBuilder.put(ALPHA_CONVERSATION, PrettyPrinter.RED)
				.put(FIX_POINT, PrettyPrinter.GREEN)
				.put(LOCAL, PrettyPrinter.PURPLE)
				.put(NEW_DEBUG_NAV_GHC, PrettyPrinter.BLUE)
				.put(UNFOLD_FOLD_INLINE, PrettyPrinter.YELLOW)
				.put(MISCELLANEOUS, PrettyPrinter.GRAY)
				.build();
	}

	/**
	 * Initializes {@link #KEYWORD_MAP} by mapping {@link Keyword} names to their
	 * respective instances.
	 * @return a {@link Map} of {@code Keyword} names to their instances.
	 */
	private static Map<String, Keyword> mapKeywords() {
		ImmutableMap.Builder<String, Keyword> keywordBuilder = ImmutableMap.builder();
		Field[] fields = CommandDispatcher.class.getDeclaredFields();
		for (Field f : fields) {
			try {
				if (f.getType().equals(Keyword.class)) {
					keywordBuilder.put(((Keyword) f.get(Keyword.class)).getKeywordName(), (Keyword) f.get(Keyword.class));
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return keywordBuilder.build();
	}

	/**
	 * Provides instructions (via the {@link #run(String...)} method) for the console to
	 * execute when the user enters a command. A {@code Command} can have any number
	 * of arguments and may accept at least a certain number of arguments if it is
	 * initialized with a lower argument bound.
	 */
	public static abstract class Command {
		/** The real {@link Command} name. */
		private String mCommandName;

		/** The {@link Command} name's alias. Useful for referencing {@code Command}
		 * names with special characters such as {@code >>>}. */
		private String mCommandAlias;

		/** The name of the group to which this {@link Command} belongs. */
		private String mGroupName;

		/** The number of arguments that this {@link Command} takes. If {@link
		 * #mLowerArgBound} is {@code true}, {@code mArgsCount} specifies the
		 * <em>minimum</em> number of arguments that this {@code Command} can take. */
		private int mArgsCount;

		/** {@code true } if {@link #mArgsCount} is a minimum amount, {@code false} if
		 * {@code mArgsCount} is a precise quantity. */
		private boolean mLowerArgBound = false;

		/**
		 * Constructs a new instance.
		 * @param commandName The name of the {@link Command}.
		 * @param groupName The group name to associate this {@code Command} with.
		 * @param argsCount The number of arguments that this {@code Command} must take.
		 */
		public Command(String commandName, String groupName, int argsCount) {
			mCommandName = commandName;
			mGroupName = groupName;
			mArgsCount = argsCount;
		}

		/**
		 * Constructs a new instance, specifying if the {@link Command} has a lower
		 * argument bound.
		 * @param commandName The name of the {@code Command}.
		 * @param groupName The group name to associate this {@code Command} with.
		 * @param argsCount The number of arguments that this {@code Command} must
		 * take. If {@code lowerArgBound} is {@code true}, this is a minimum amount.
		 * @param lowerArgBound {@code true} if {@code argsCount} is a minimum
		 * amount, {@code false} if {@code argsCount} is a precise quantity.
		 */
		public Command(String commandName, String groupName, int argsCount, boolean lowerArgBound) {
			this(commandName, groupName, argsCount);
			mLowerArgBound = lowerArgBound;
		}

		/**
		 * Constructs a new instance that has an alias.
		 * @param commandName The real name of the {@link Command}.
		 * @param commandAlias The {@code Command}'s alias.
		 * @param groupName The group name to associate this {@code Command} with.
		 * @param argsCount The number of arguments that this {@code Command} must take.
		 */
		public Command(String commandName, String commandAlias, String groupName, int argsCount) {
			this(commandName, groupName, argsCount);
			mCommandAlias = commandAlias;
		}

		/**
		 * Constructs a new instance, specifying a {@link Command} alias and if the
		 * {@code Command} has a lower argument bound.
		 * argument bound.
		 * @param commandName The real name of the {@code Command}.
		 * @param commandAlias The {@code Command}'s alias.
		 * @param groupName The group name to associate this {@code Command} with.
		 * @param argsCount The number of arguments that this {@code Command} must
		 * take. If {@code lowerArgBound} is {@code true}, this is a minimum amount.
		 * @param lowerArgBound {@code true} if {@code argsCount} is a minimum
		 * amount, {@code false} if {@code argsCount} is a precise quantity.
		 */
		public Command(String commandName, String commandAlias,
				String groupName, int argsCount, boolean lowerArgBound) {
			this(commandName, commandAlias, groupName, argsCount);
			mCommandAlias = commandAlias;
		}

		/**
		 * Returns the true name of the {@link Command}.
		 * @return the real (not aliased) name of the {@code Command}.
		 */
		public String getCommandName() {
			return mCommandName;
		}

		/**
		 * Returns the {@link Command} alias.
		 * @return the {@code Command} alias, or {@code null} if there is none.
		 */
		public String getCommandAlias() {
			return mCommandAlias;
		}

		/**
		 * Returns the name of the group this {@link Command} is associated with.
		 * @return this {@code Command}'s group name.
		 */
		public String getGroupName() {
			return mGroupName;
		}

		/**
		 * Return the (possibly minimum) number of arguments that this {@link Command}
		 * must take.
		 * @return the number of arguments that the {@code Command} takes.
		 */
		public int getArgsCount() {
			return mArgsCount;
		}

		/**
		 * Returns whether the {@link Command} accepts a minimum number of arguments.
		 * @return if the {@code Command} has a lower argument bound.
		 */
		public boolean hasLowerArgBound() {
			return mLowerArgBound;
		}

		/**
		 * The instructions that are ran when this {@link Command} is run on the console.
		 * @param args Parameters that the {@code Command} uses.
		 */
		protected abstract void run(String... args);
	}

	/**
	 * As opposed to a {@link Command}, a {@code Keyword} is a word that {@link 
	 * PrettyPrinter} singles out as important (by coloring it). When a {@code Keyword}
	 * is accessed by long-clicking a {@link ConsoleListView} entry and selecting it via
	 * the context menu, a corresponding {@code Command} is run.
	 */
	public static class Keyword {
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
				mCommand = TOAST;
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

}
