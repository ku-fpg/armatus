package edu.kufpg.armatus.console;

import java.util.List;
import java.util.Map;
import java.util.Set;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;

import edu.kufpg.armatus.data.CommandInfo;

public class CommandHolder {
	public static final String COMMONLY_USED_COMMANDS_TAG = "Commonly used";
	private static final Set<String> COMMONLY_USED_COMMANDS = createDefaultCommonlyUsedCommandNames();

	private static List<String> sTags = ImmutableList.of(CustomCommandDispatcher.CLIENT_COMMANDS_TAG);
	private static ListMultimap<String, String> sTagCommandNames = CustomCommandDispatcher.getTagCommandNames();
	private static ListMultimap<String, ? extends CommandInfo> sNameCommands = createDefaultNameCommands();

	private CommandHolder() {}

	public static String getTag(int index) {
		return sTags.get(index);
	}

	public static int getTagCount() {
		return sTags.size();
	}

	public static List<String> getCommandNamesFromTag(String tagName) {
		return sTagCommandNames.get(tagName);
	}

	public static int getCommandTypeSigCount(String commandName) {
		return sNameCommands.get(commandName).size();
	}

	public static List<? extends CommandInfo> getCommandsFromName(String commandName) {
		return sNameCommands.get(commandName);
	}

	public static boolean isCommonlyUsedCommand(String commandName) {
		return COMMONLY_USED_COMMANDS.contains(commandName);
	}

	static List<String> getTags() {
		return sTags;
	}

	static ListMultimap<String, String> getTagCommandNames() {
		return sTagCommandNames;
	}

	static void setTags(Iterable<? extends String> tagList) {
		ImmutableList.Builder<String> builder = ImmutableList.builder();
		if (tagList != null) {
			builder.addAll(tagList);
		}
		sTags = builder.add(CustomCommandDispatcher.CLIENT_COMMANDS_TAG).build();
	}

	static void setTagCommandNames(Multimap<? extends String, ? extends String> tagMap) {
		ImmutableListMultimap.Builder<String, String> builder = ImmutableListMultimap.builder();
		if (tagMap != null) {
			builder.putAll(tagMap);
		}
		sTagCommandNames = builder.putAll(CustomCommandDispatcher.getTagCommandNames()).build();
	}

	static void setCommandInfos(Multimap<? extends String, ? extends CommandInfo> commandMap) {
		ImmutableListMultimap.Builder<String, CommandInfo> builder = ImmutableListMultimap.builder();
		if (commandMap != null) {
			builder.putAll(commandMap);
		}
		for (Map.Entry<String, CustomCommandInfo> entry : CustomCommandDispatcher.getCommandNameInfos().entrySet()) {
			builder.put(entry);
		}
		sNameCommands = builder.build();
	}

	private static Set<String> createDefaultCommonlyUsedCommandNames() {
		return ImmutableSet.of("alpha","alpha-alt","alpha-case",
				"alpha-case-binder","alpha-lam","alpha-let","alpha-top",
				"info","top","bash","simplify","unshadow","{","}",
				"set-pp","set-pp-coerion","set-pp-renderer","set-pp-type","set-pp-width");
	}

	private static ListMultimap<String, ? extends CommandInfo> createDefaultNameCommands() {
		ImmutableListMultimap.Builder<String, CommandInfo> builder = ImmutableListMultimap.builder();
		for (Map.Entry<String, CustomCommandInfo> entry : CustomCommandDispatcher.getCommandNameInfos().entrySet()) {
			builder.put(entry);
		}
		return builder.build();
	}
}
