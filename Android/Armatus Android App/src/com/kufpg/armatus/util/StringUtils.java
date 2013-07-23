package com.kufpg.armatus.util;

import android.text.Editable;

public class StringUtils {

	public static final String WHITESPACE = "\\s+";
	public static final String NBSP = "\u00A0";

	public static String applyCharWrap(String str) {
		return str.replace(" ", NBSP);
	}

	public static Editable applyCharWrap(Editable editable) {
		for (int i = 0; i < editable.length(); i++) {
			if (editable.charAt(i) == ' ') {
				editable.replace(i, i+1, NBSP);
			}
		}
		return editable;
	}

	public static String getLeadingWhitespace(String str) {
		int index = 0;
		while (index < str.length() && str.substring(index, index+1).matches(WHITESPACE)) {
			index++;
		}
		return str.substring(0, index);
	}

	public static String getTrailingWhitespace(String str) {
		int index = str.length();
		while (index > 0 && str.substring(index-1, index).matches(WHITESPACE)) {
			index--;
		}
		return str.substring(index);
	}

	public static String removeCharWrap(String str) {
		return str.replace(NBSP, " ");
	}

	public static Editable removeCharWrap(Editable editable) {
		for (int i = 0; i < editable.length(); i++) {
			if (editable.charAt(i) == '\u00A0') {
				editable.replace(i, i+1, " ");
			}
		}
		return editable;
	}

}
