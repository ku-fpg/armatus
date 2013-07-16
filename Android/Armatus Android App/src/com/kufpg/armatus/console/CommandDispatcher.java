package com.kufpg.armatus.console;

import java.lang.reflect.Field;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import org.json.JSONObject;

import com.google.common.collect.ImmutableMap;
import com.kufpg.armatus.BaseActivity;
import com.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import com.kufpg.armatus.server.HermitServer;
import com.kufpg.armatus.util.NetworkUtils;

import android.content.Intent;
import android.util.Log;
import android.widget.Toast;

@SuppressWarnings("unused")
public class CommandDispatcher {
	public static final String ALPHA_CONVERSATION = "Alpha Conversation";
	public static final String FIX_POINT = "Fix Point";
	public static final String LOCAL = "Local";
	public static final String NEW_DEBUG_NAV_GHC = "New/Debug/Nav/GHC";
	public static final String UNFOLD_FOLD_INLINE = "Unfold/Fold/Inline";
	public static final String MISCELLANEOUS = "Miscellaneous";

	private static ConsoleActivity mConsole;

	//List of Commands
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
	private static final Map<String, Command> COMMAND_MAP = mapCommands();
	private static final Map<String, String> GROUP_COLOR_MAP = mapGroupColors();
	private static final Map<String, String> ALIASED_COMMAND_MAP = mapAliasedCommands();

	//List of Keywords
	private static final Keyword RED = new Keyword("red", "toast", PrettyPrinter.RED);
	private static final Keyword GREEN = new Keyword("green", "toast", PrettyPrinter.GREEN);
	private static final Keyword BLUE = new Keyword("blue", "toast", PrettyPrinter.BLUE);
	private static Map<String, Keyword> KEYWORD_MAP = mapKeywords();

	public CommandDispatcher(ConsoleActivity console) {
		mConsole = console;
	}

	public void runOnConsole(String commandName, String... args) {
		Command command = COMMAND_MAP.get(commandName);
		if (command != null) {
			runOnConsole(command, args);
		} else {
			mConsole.appendConsoleEntry("Error: " + commandName + " is not a valid command.");
		}
	}

	private void runOnConsole(Command command, String... args) {
		String commandString = command.getCommandName()
				+ " " + varargsToString(args);
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

	public void runKeywordCommand(String keywordName, String arg) {
		Keyword keyword = KEYWORD_MAP.get(keywordName);
		if (keyword != null) {
			runOnConsole(keyword.getCommand(), arg);
		} else {
			mConsole.appendConsoleEntry("Error: " + keyword + " is not a valid keyword.");
		}
	}

	public static boolean isAlias(String commandName) {
		return ALIASED_COMMAND_MAP.containsKey(commandName);
	}

	public static boolean isCommand(String commandName) {
		return COMMAND_MAP.containsKey(commandName);
	}

	public static boolean isKeyword(String keywordName) {
		return KEYWORD_MAP.containsKey(keywordName);
	}

	public static Command getCommand(String commandName) {
		return COMMAND_MAP.get(commandName);
	}
	
	static SortedSet<String> getCommandNames() {
		SortedSet<String> commandNames = new TreeSet<String>();
		commandNames.addAll(COMMAND_MAP.keySet());
		return commandNames;
	}

	public static String getGroupColor(String groupName) {
		return GROUP_COLOR_MAP.get(groupName);
	}

	public static Keyword getKeyword(String keywordName) {
		return KEYWORD_MAP.get(keywordName);
	}

	public static String unaliasCommand(String alias) {
		return ALIASED_COMMAND_MAP.get(alias);
	}

	private static String varargsToString(String... varargs) {
		String newString = "";
		for(String string : varargs) {
			newString += string + " ";
		}
		newString.trim();
		return newString;
	}

	private static Map<String, Command> mapCommands() {
		ImmutableMap.Builder<String, Command> commandBuilder = ImmutableMap.builder();
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
	 * A Command is a series of instructions that is ran when run(args) is called.
	 * A Command can have any number of arguments and may accept at least a
	 * certain number of arguments is it is initialized with a lowerArgBound.
	 */
	public static abstract class Command {

		private String mCommandName;
		private String mCommandAlias;
		private String mGroupName;
		private int mArgsCount;
		private boolean mLowerArgBound = false;

		public Command(String commandName, String groupName, int argsCount) {
			mCommandName = commandName;
			mGroupName = groupName;
			mArgsCount = argsCount;
		}

		public Command(String commandName, String groupName, int argsCount, boolean lowerArgBound) {
			this(commandName, groupName, argsCount);
			mLowerArgBound = lowerArgBound;
		}

		public Command(String commandName, String commandAlias, String groupName, int argsCount) {
			this(commandName, groupName, argsCount);
			mCommandAlias = commandAlias;
		}

		/**
		 * Use this constructor for commands with special characters in
		 * their names (e.g., >>>).
		 */
		public Command(String commandName, String commandAlias,
				String groupName, int argsCount, boolean lowerArgBound) {
			this(commandName, commandAlias, groupName, argsCount);
			mCommandAlias = commandAlias;
		}

		public String getCommandName() {
			return mCommandName;
		}

		public String getCommandAlias() {
			return mCommandAlias;
		}

		public String getGroupName() {
			return mGroupName;
		}

		public int getArgsCount() {
			return mArgsCount;
		}

		public boolean hasLowerArgBound() {
			return mLowerArgBound;
		}

		protected abstract void run(String... args);
	}

	/**
	 * As opposed to a Command, a Keyword is a word that ConsoleTextView.PrettyPrinter
	 * singles out as important (by coloring it). When a keyword is accessed by long-
	 * clicking a ConsoleTextView, a corresponding command is run.
	 */
	public static class Keyword {

		private String mKeywordName;
		private Command mCommand;
		private String mColor;

		public Keyword(String keywordName, String commandName, String color) {
			mKeywordName = keywordName;
			if (isCommand(commandName)) {
				mCommand = COMMAND_MAP.get(commandName);
			} else {
				mCommand = TOAST;
			}
			mColor = color;
		}

		public String getKeywordName() {
			return mKeywordName;
		}

		public Command getCommand() {
			return mCommand;
		}

		public String getColor() {
			return mColor;
		}
	}

}
