package com.kufpg.androidhermit.util;

import java.lang.reflect.Field;
import java.util.concurrent.ConcurrentHashMap;

import com.kufpg.androidhermit.TestConsoleActivity;

public class CommandDispatcher {

	private static Command clear = new Command("clear", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.clear();
		}
	};
	private static Command consider = new Command("consider", 1, false) {
		@Override
		protected void run(String... args) {
			mConsole.addMessage("TODO: Figure out what consider " + args[0] + " does.");
		}
	};
	private static Command resume = new Command("resume", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.addMessage("TODO: Figure out what resume does.");
		}
	};
	private static Command exit = new Command("exit", 0, false) {
		@Override
		protected void run(String... args) {
			mConsole.exit();
		}
	};

	private static TestConsoleActivity mConsole;
	private static ConcurrentHashMap<String, Command> commandMap = new ConcurrentHashMap<String, Command>();

	public CommandDispatcher(TestConsoleActivity console) {
		mConsole = console;

		//This gets all of this class's instance variables and puts the variables
		//of type Command into a HashMap for easy access
		Field[] fields = CommandDispatcher.class.getDeclaredFields();
		for (Field f : fields) {
			if (f.getType().equals(Command.class)) {
				try {
					commandMap.put(f.getName(), (Command) f.get(Command.class));
				} catch (IllegalArgumentException e) {
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				}
			} 
		}
	}

	private void execute(Command command, String... args) {
		String commandString = command.getCommandName()
				+ " " + varargsToString(args);
		mConsole.addMessage(commandString);

		if (command.hasLowerArgBound()) {
			if (args.length < command.getMinArgs()) {
				mConsole.addMessage("Error: " + command.getCommandName() +
						" requires at least " + command.getMinArgs() +
						(command.getMinArgs() == 1 ? " argument." :
								" arguments."));
				return;
			}
		} else if (args.length != command.getMinArgs()) {
			mConsole.addMessage("Error: " + command.getCommandName() +
					" requires exactly " + command.getMinArgs() +
					(command.getMinArgs() == 1 ? " argument." :
							" arguments."));
			return;
		}
		command.run(args);
	}

	public void execute(String commandName, String... args) {
		execute(commandMap.get(commandName), args);
	}

	public boolean isCommand(String commandName) {
		return commandMap.containsKey(commandName);
	}

	private String varargsToString(String... varargs) {
		String newString = "";
		for(String string : varargs) {
			newString += string + " ";
		}
		newString.trim();
		return newString;
	}
}
