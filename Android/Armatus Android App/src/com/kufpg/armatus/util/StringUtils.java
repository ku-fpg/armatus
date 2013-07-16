package com.kufpg.armatus.util;

public class StringUtils {
	
	public static final String WHITESPACE = "\\s+";
	public static final String NBSP = "\u00A0";
	
	public static String applyCharWrap(String str) {
		return str.replace(" ", NBSP);
	}
	
	public static String removeCharWrap(String str) {
		return str.replace(NBSP, " ");
	}

}
