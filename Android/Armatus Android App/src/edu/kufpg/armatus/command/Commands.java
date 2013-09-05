package edu.kufpg.armatus.command;

import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.google.common.collect.ListMultimap;

public class Commands {
	private static List<String> sTagList = Arrays.asList(CustomCommandDispatcher.CLIENT_COMMANDS_TAG);
	private static ListMultimap<String, String> sTagMap;
	private static Map<String, String> sCommandInfoMap;
	
	private Commands() {}
	
	public static List<String> getTagList() {
		return sTagList;
	}
	
	public static ListMultimap<String, String> getTagMap() {
		return sTagMap;
	}
	
	public static Map<String, String> getCommandInfoMap() {
		return sCommandInfoMap;
	}
	
	public static void setTagList(List<String> tagList) {
		sTagList = tagList;
	}
	
	public static void setTagMap(ListMultimap<String, String> tagMap) {
		sTagMap = tagMap;
	}
	
	public static void setCommandInfoMap(Map<String, String> commandInfoMap) {
		sCommandInfoMap = commandInfoMap;
	}
	
}
