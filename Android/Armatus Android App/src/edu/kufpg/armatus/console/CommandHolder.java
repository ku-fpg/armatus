package edu.kufpg.armatus.console;

import java.util.List;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import edu.kufpg.armatus.data.CommandInfo;

public class CommandHolder {
	private static List<String> sTagList = ImmutableList.of(CustomCommandDispatcher.CLIENT_COMMANDS_TAG);
	private static ListMultimap<String, ? extends CommandInfo> sTagMap = CustomCommandDispatcher.getCustomCommandTagMap();
	
	private CommandHolder() {}
	
	public static String getTag(int index) {
		return sTagList.get(index);
	}
	
	public static int getTagCount() {
		return sTagList.size();
	}
	
	public static List<? extends CommandInfo> getTagCommands(String tagName) {
		return sTagMap.get(tagName);
	}
	
	static void setTagList(Iterable<? extends String> tagList) {
		ImmutableList.Builder<String> tagListBuilder = ImmutableList.builder();
		tagListBuilder.add("Commonly Used").addAll(tagList).add(CustomCommandDispatcher.CLIENT_COMMANDS_TAG);
		sTagList = tagListBuilder.build();
	}
	
	static void setTagMap(Multimap<? extends String, ? extends CommandInfo> tagMap) {
		ImmutableListMultimap.Builder<String, CommandInfo> tagMapBuilder = ImmutableListMultimap.builder();
		tagMapBuilder.putAll(tagMap).putAll(CustomCommandDispatcher.getCustomCommandTagMap());
		sTagMap = tagMapBuilder.build();
	}
	
}
