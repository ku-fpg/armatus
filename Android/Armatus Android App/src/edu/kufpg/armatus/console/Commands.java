package edu.kufpg.armatus.console;

import java.util.List;
import java.util.SortedSet;

import com.google.common.base.Function;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;

import edu.kufpg.armatus.networking.data.CommandInfo;

public class Commands {
	private static List<String> sTagList = ImmutableList.of(CustomCommandDispatcher.CLIENT_COMMANDS_TAG);
	private static ListMultimap<String, ? extends CommandInfo> sTagMap = CustomCommandDispatcher.getCommandTagsMap();
	private static SortedSet<String> sCommandSet = getCustomCommandSet();
	
	private Commands() {}
	
	public static String getTag(int index) {
		return sTagList.get(index);
	}
	
	public static int getTagCount() {
		return sTagList.size();
	}
	
	public static List<? extends CommandInfo> getTagCommands(String tagName) {
		return sTagMap.get(tagName);
	}
	
	static SortedSet<String> getCommands() {
		return sCommandSet;
	}
	
	static void setTagList(Iterable<? extends String> tagList) {
		ImmutableList.Builder<String> tagListBuilder = ImmutableList.builder();
		tagListBuilder.addAll(tagList).add(CustomCommandDispatcher.CLIENT_COMMANDS_TAG);
		sTagList = tagListBuilder.build();
	}
	
	static void setTagMap(Multimap<? extends String, ? extends CommandInfo> tagMap) {
		ImmutableListMultimap.Builder<String, CommandInfo> tagMapBuilder = ImmutableListMultimap.builder();
		tagMapBuilder.putAll(tagMap).putAll(CustomCommandDispatcher.getCommandTagsMap());
		sTagMap = tagMapBuilder.build();
	}
	
	static void setCommandSet(Iterable<? extends String> commandSet) {
		ImmutableSortedSet.Builder<String> commandSetBuilder = ImmutableSortedSet.naturalOrder();
		commandSetBuilder.addAll(commandSet).addAll(getCustomCommandSet());
		sCommandSet = commandSetBuilder.build();
	}
	
	private static SortedSet<String> getCustomCommandSet() {
		return ImmutableSortedSet.copyOf(Collections2.transform(CustomCommandDispatcher.getCustomCommandMap().values(),
				new Function<CustomCommandInfo, String>() {
			@Override
			public String apply(CustomCommandInfo info) {
				return info.getName();
			}
		}));
	}
	
}
