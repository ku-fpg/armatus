package edu.kufpg.armatus.console;

import java.util.List;
import java.util.Map;
import java.util.NavigableSet;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;

public class Commands {
	private static List<String> sTagList = ImmutableList.of(CustomCommandDispatcher.CLIENT_COMMANDS_TAG);
	private static ListMultimap<String, String> sTagMap = CustomCommandDispatcher.getCommandTagsMap();
	private static Map<String, String> sCommandHelpMap = CustomCommandDispatcher.getCommandInfoMap();
	private static NavigableSet<String> sCommandSet = CustomCommandDispatcher.getCommandInfoMap().navigableKeySet();
	
	private Commands() {}
	
	public static String getTag(int index) {
		return sTagList.get(index);
	}
	
	public static int getTagCount() {
		return sTagList.size();
	}
	
	public static List<String> getTagCommands(String tagName) {
		return sTagMap.get(tagName);
	}
	
	public static String getCommandHelp(String commandName) {
		return sCommandHelpMap.get(commandName);
	}
	
	static NavigableSet<String> getCommands() {
		return sCommandSet;
	}
	
	static void setTagList(Iterable<? extends String> tagList) {
		ImmutableList.Builder<String> tagListBuilder = ImmutableList.builder();
		tagListBuilder.addAll(tagList).add(CustomCommandDispatcher.CLIENT_COMMANDS_TAG);
		sTagList = tagListBuilder.build();
	}
	
	static void setTagMap(Multimap<? extends String, ? extends String> tagMap) {
		ImmutableListMultimap.Builder<String, String> tagMapBuilder = ImmutableListMultimap.builder();
		tagMapBuilder.putAll(tagMap).putAll(CustomCommandDispatcher.getCommandTagsMap());
		sTagMap = tagMapBuilder.build();
	}
	
	static void setCommandHelpMap(Map<? extends String, ? extends String> commandInfoMap) {
		ImmutableMap.Builder<String, String> commandHelpBuilder = ImmutableMap.builder();
		commandHelpBuilder.putAll(commandInfoMap).putAll(CustomCommandDispatcher.getCommandInfoMap());
		sCommandHelpMap = commandHelpBuilder.build();
	}
	
	static void setCommandSet(Iterable<? extends String> commandSet) {
		ImmutableSortedSet.Builder<String> commandSetBuilder = ImmutableSortedSet.naturalOrder();
		commandSetBuilder.addAll(commandSet).addAll(CustomCommandDispatcher.getCommandInfoMap().navigableKeySet());
		sCommandSet = commandSetBuilder.build();
	}
	
}
