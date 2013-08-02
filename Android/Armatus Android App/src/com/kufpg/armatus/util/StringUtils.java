package com.kufpg.armatus.util;

import android.text.Editable;

/**
 * Utility class containing methods useful for string analysis and transformation.
 */
public class StringUtils {

	/**
	 * A regular expression that matches several kinds of whitespace characters, including
	 * {@link #NBSP} and newlines.
	 */
	public static final String WHITESPACE = "\\s+";
	
	/**
	 * A non-breaking space string. Using this instead of a regular space string (" ") will
	 * prevent {@link android.widget.TextView TextViews} from applying their normal
	 * line-breaking behavior.
	 */
	public static final String NBSP = "\u00A0";
	
	/**
	 * A non-breaking space character. Using this instead of a regular space character (' ')
	 * will prevent {@link android.widget.TextView TextViews} from applying their normal
	 * line-breaking behavior.
	 */
	public static final char NBSP_CHAR = '\u00A0';
	
	private StringUtils() {}

	/**
	 * Returns the whitespace characters at the beginning of a string.
	 * @param str The string whose leading whitespace should be found.
	 * @return a string containing the characters making up the leading whitespace of the
	 * string. If there is no whitespace there, the empty string ("") is returned.
	 */
	public static String getLeadingWhitespace(String str) {
		int index = 0;
		while (index < str.length() && str.substring(index, index+1).matches(WHITESPACE)) {
			index++;
		}
		return str.substring(0, index);
	}

	/**
	 * Returns the whitespace characters at the end of a string.
	 * @param str The string whose trailing whitespace should be found.
	 * @return a string containing the characters making up the trailing whitespace of the
	 * string. If there is no whitespace there, the empty string ("") is returned.
	 */
	public static String getTrailingWhitespace(String str) {
		int index = str.length();
		while (index > 0 && str.substring(index-1, index).matches(WHITESPACE)) {
			index--;
		}
		return str.substring(index);
	}
	
	/**
	 * Returns a string with all regular spaces replaced by non-breaking spaces.
	 * @param str The string to apply character wrap to.
	 * @return a string with all spaces replaced by {@link #NBSP}.
	 */
	public static String withCharWrap(String str) {
		return str.replace(" ", NBSP);
	}

	/**
	 * Returns an {@link Editable} with all regular spaces replaced by non-breaking spaces.
	 * @param editable The {@code Editable} object to apply character wrap to.
	 * @return an {@code Editable} object with all spaces replaced by {@link #NBSP}.
	 */
	public static Editable withCharWrap(Editable editable) {
		for (int i = 0; i < editable.length(); i++) {
			if (editable.charAt(i) == ' ') {
				editable.replace(i, i+1, NBSP);
			}
		}
		return editable;
	}

	/**
	 * Returns a string with all non-breaking spaces replaced by regular spaces.
	 * @param str The string from which character wrap should be removed.
	 * @return a string with all non-breaking spaces replaced by regular ones.
	 */
	public static String withoutCharWrap(String str) {
		return str.replace(NBSP, " ");
	}

	/**
	 * Returns an {@link Editable} with all non-breaking spaces replaced by regular spaces.
	 * @param str The {@code Editable} object from which character wrap should be removed.
	 * @return an {@code Editable} object with all non-breaking spaces replaced by regular
	 * ones.
	 */
	public static Editable withoutCharWrap(Editable editable) {
		for (int i = 0; i < editable.length(); i++) {
			if (editable.charAt(i) == NBSP_CHAR) {
				editable.replace(i, i+1, " ");
			}
		}
		return editable;
	}

}
