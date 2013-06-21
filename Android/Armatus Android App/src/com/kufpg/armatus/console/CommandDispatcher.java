package com.kufpg.armatus.console;

import java.lang.reflect.Field;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import org.json.JSONObject;

import com.kufpg.armatus.BaseActivity;
import com.kufpg.armatus.console.CommandDispatcher.Command;
import com.kufpg.armatus.console.ConsoleActivity.ConsoleEntryAdder;
import com.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import com.kufpg.armatus.server.HermitServer;
import com.kufpg.armatus.util.NetworkUtils;

import android.content.Intent;
import android.util.Log;
import android.widget.Toast;

@SuppressWarnings("unused")
public class CommandDispatcher {
	private static ConsoleActivity mConsole;

	//List of Commands
	private static Command alpha = new Command("alpha", "Alpha Conversation", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command alphaAlt = new Command("alpha-alt", "Alpha Conversation", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command alphaCase = new Command("alpha-case", "Alpha Conversation", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command alphaCaseBinder = new Command("alpha-case-binder", "Alpha Conversation", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command alphaLam = new Command("alpha-lam", "Alpha Conversation", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command alphaLet = new Command("alpha-let", "Alpha Conversation", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command alphaTop = new Command("alpha-top", "Alpha Conversation", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command undershadow = new Command("undershadow", "Alpha Conversation", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static Command fixComputation = new Command("fix-computation", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command fixIntro = new Command("fix-intro", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command rollingRule = new Command("rolling-rule", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command wwAssumptionA = new Command("ww-assumption-a", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command wwAssumptionB = new Command("ww-assumption-b", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command wwAssumptionC = new Command("ww-assumption-c", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command wwFactorisation = new Command("ww-factorisation", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command wwFusion = new Command("ww-fusion", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command wwSplit = new Command("ww-split", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command wwSplitParam = new Command("ww-split-param", "Fix Point", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static Command abstract_ = new Command("abstract", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command betaExpand = new Command("beta-expand", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command betaReduce = new Command("beta-reduce", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command betaReducePlus = new Command("beta-reduce-plus", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command etaExpand = new Command("eta-expand", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command etaReduce = new Command("eta-reduce", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command flattenModule = new Command("flatten-module", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command flattenProgram = new Command("flatten-program", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command nonrecToRec = new Command("nonrec-to-rec", "Local", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static Command addRule = new Command("add-rule", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command anyCall = new Command("any-call", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command applyRule = new Command("apply-rule", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command applyRules = new Command("apply-rules", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command bindingGroupOf = new Command("binding-group-of", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command castEliminate = new Command("cast-eliminate", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command cleanupUnfold = new Command("cleanup-unfold", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command compareValues = new Command("compare-values", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command consider = new Command("consider", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command considerConstruct = new Command("consider-construct", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command considerName = new Command("consider-name", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command debugUnfold = new Command("debug-unfold", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command deshadowProgram = new Command("deshadow-program", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command freeIds = new Command("free-ids", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command info = new Command("info", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command inlineAll = new Command("inline-all", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command letSub = new Command("let-sub", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command letTuple = new Command("let-tuple", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command observe = new Command("observe", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command observeFail = new Command("observe-fail", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command occureAnalysis = new Command("occure-analysis", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command push = new Command("push", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command resume = new Command("resume", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command rewrites = new Command(">>>", "rewrites", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command rewritesOneFail = new Command(">+>", "rewrites-one-fail", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command rhsOf = new Command("rhs-of", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command safeLetSub = new Command("safe-let-sub", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command simplify = new Command("simplify", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command staticArgument = new Command("static-argument", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command test = new Command("test", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command trace = new Command("trace", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command translateRewrite = new Command("<+", "translate-rewrite", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command unfoldRule = new Command("unfold-rule", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command unsafeReplace = new Command("unsafe-replace", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command variable = new Command("variable", "New/Debug/Nav/GHC", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static Command fold = new Command("fold", "Unfold/Fold/Inline", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command foldRemembered = new Command("fold-remembered", "Unfold/Fold/Inline", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command inline = new Command("inline", "Unfold/Fold/Inline", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command inlineCaseBinder = new Command("inline-case-binder", "Unfold/Fold/Inline", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command inlineName = new Command("inline-name", "Unfold/Fold/Inline", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command inlineScrutinee = new Command("inline-scrutinee", "Unfold/Fold/Inline", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command remember = new Command("remember", "Unfold/Fold/Inline", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};
	private static Command unfold = new Command("unfold", "Unfold/Fold/Inline", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.appendConsoleEntry("This is a " + getGroupName() + " command.");
		}
	};

	private static Command clear = new Command("clear", "Miscellaneous", 0, true) {
		@Override
		protected void run(String... args) {
			mConsole.clear();
		}
	};
	private static Command exit = new Command("exit", "Miscellaneous", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.exit();
		}
	};
	private static Command serverTest = new Command("server-test", "Miscellaneous", 0, false) {
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
	private static Command toast = new Command("toast", "Miscellaneous", 0, true) {
		@Override
		protected void run(String... args) {
			Toast theToast = null;
			if (args.length == 0) {
				theToast = Toast.makeText(mConsole, "No arguments!", Toast.LENGTH_SHORT);
			} else {
				theToast = Toast.makeText(mConsole,
						varargsToString(args), Toast.LENGTH_SHORT);
			}
			theToast.show();
		}
	};
	private static Command terminal = new Command("terminal", "Miscellaneous", 0, true){
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
	private static Map<String, Command> mCommandMap = new HashMap<String, Command>();
	private static Map<String, String> mGroupColorMap = new HashMap<String, String>();
	private static Map<String, String> mAliasedCommandMap = new HashMap<String, String>();

	//List of Keywords
	private static Keyword red = new Keyword("red", "toast", PrettyPrinter.RED);
	private static Keyword green = new Keyword("green", "toast", PrettyPrinter.GREEN);
	private static Keyword blue = new Keyword("blue", "toast", PrettyPrinter.BLUE);
	private static Map<String, Keyword> mKeywordMap = new HashMap<String, Keyword>();

	public CommandDispatcher(ConsoleActivity console) {
		mConsole = console;
		mapInstances();

		mGroupColorMap.put("Alpha Conversation", PrettyPrinter.RED);
		mGroupColorMap.put("Fix Point", PrettyPrinter.GREEN);
		mGroupColorMap.put("Local", PrettyPrinter.PURPLE);
		mGroupColorMap.put("New/Debug/Nav/GHC", PrettyPrinter.BLUE);
		mGroupColorMap.put("Unfold/Fold/Inline", PrettyPrinter.YELLOW);
		mGroupColorMap.put("Miscellaneous", PrettyPrinter.GRAY);
	}

	public void runOnConsole(String commandName, String... args) {
		Command command = mCommandMap.get(commandName);
		if (command != null) {
			runOnConsole(command, args);
		} else {
			mConsole.appendConsoleEntry("Error: " + commandName + " is not a valid command.");
		}
	}

	private void runOnConsole(Command command, String... args) {
		String commandString = command.getCommandName()
				+ " " + varargsToString(args);
		//mConsole.addConsoleEntry(commandString);
		ConsoleEntryAdder edit = mConsole.new ConsoleEntryAdder(commandString);
		BaseActivity.getEditManager().applyEdit(edit);
		mConsole.addCommandEntry(command.getCommandName());

		if (command.hasLowerArgBound()) {
			if (args.length < command.getArgsNum()) {
				mConsole.appendConsoleEntry("Error: " + command.getCommandName() +
						" requires at least " + command.getArgsNum() +
						(command.getArgsNum() == 1 ? " argument." :
								" arguments."));
				return;
			}
		} else if (args.length != command.getArgsNum()) {
			mConsole.appendConsoleEntry("Error: " + command.getCommandName() +
					" requires exactly " + command.getArgsNum() +
					(command.getArgsNum() == 1 ? " argument." :
							" arguments."));
			return;
		}
		command.run(args);
	}

	public void runKeywordCommand(String keywordName, String arg) {
		Keyword keyword = mKeywordMap.get(keywordName);
		if (keyword != null) {
			runOnConsole(keyword.getCommand(), arg);
		} else {
			mConsole.appendConsoleEntry("Error: " + keyword + " is not a valid keyword.");
		}
	}

	public static boolean isAlias(String commandName) {
		return mAliasedCommandMap.containsKey(commandName);
	}

	public static boolean isCommand(String commandName) {
		return mCommandMap.containsKey(commandName);
	}

	public static boolean isKeyword(String keywordName) {
		return mKeywordMap.containsKey(keywordName);
	}

	public static Command getCommand(String commandName) {
		return mCommandMap.get(commandName);
	}

	public static String getGroupColor(String groupName) {
		return mGroupColorMap.get(groupName);
	}

	public static Keyword getKeyword(String keywordName) {
		return mKeywordMap.get(keywordName);
	}

	public static String unaliasCommand(String alias) {
		return mAliasedCommandMap.get(alias);
	}

	private static String varargsToString(String... varargs) {
		String newString = "";
		for(String string : varargs) {
			newString += string + " ";
		}
		newString.trim();
		return newString;
	}

	private static void mapInstances() {
		Field[] fields = CommandDispatcher.class.getDeclaredFields();
		for (Field f : fields) {
			try {
				if (f.getType().equals(Command.class)) {
					Command command = (Command) f.get(Command.class);
					mCommandMap.put(command.getCommandName(), command);
					if (command.getCommandAlias() != null) {
						mAliasedCommandMap.put(command.getCommandAlias(), command.getCommandName());
					}
				} else if (f.getType().equals(Keyword.class)) {
					mKeywordMap.put(((Keyword) f.get(Keyword.class)).getKeywordName(), (Keyword) f.get(Keyword.class));
				}
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
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
		private int mArgsNum;
		private boolean mLowerArgBound;

		public Command(String commandName, String groupName, int minArgs, boolean lowerArgBound) {
			mCommandName = commandName;
			mGroupName = groupName;
			mArgsNum = minArgs;
			mLowerArgBound = lowerArgBound;
		}

		/**
		 * Use this constructor for commands with special characters in
		 * their names (e.g., >>>).
		 */
		public Command(String commandName, String commandAlias,
				String groupName, int minArgs, boolean lowerArgBound) {
			this(commandName, groupName, minArgs, lowerArgBound);
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

		public int getArgsNum() {
			return mArgsNum;
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
				mCommand = mCommandMap.get(commandName);
			} else {
				mCommand = toast;
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
