package edu.kufpg.armatus.console;

import java.util.List;

import edu.kufpg.armatus.command.CustomCommandDispatcher;
import edu.kufpg.armatus.console.HermitClient.Glyph;
import edu.kufpg.armatus.console.HermitClient.GlyphStyle;
import edu.kufpg.armatus.util.StringUtils;
import android.graphics.Color;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;

/**
 * Colors text depending on its usage of {@link CustomCommandDispatcher.Keyword
 * Keywords}.
 */
public class PrettyPrinter {
	public static final String RED = "#CC060B";
	public static final String GREEN = "#1DDA1C";
	public static final String BLUE = "#0090D3";
	public static final String GRAY = "#969696";
	public static final String PURPLE = "#7300FB";
	public static final String YELLOW = "#FDFD0D";

	public static CharSequence createPrettyText(List<Glyph> glyphs) {
		SpannableStringBuilder builder = new SpannableStringBuilder();
		for (Glyph glyph : glyphs) {
			String word;
			if (!glyph.text.equals("\n")) {
				word = glyph.text + StringUtils.NBSP;
			} else {
				word = glyph.text;
			}

			SpannableString spanWord = new SpannableString(word);
			if (!glyph.style.equals(GlyphStyle.NORMAL)) {
				spanWord.setSpan(
						new ForegroundColorSpan(Color
								.parseColor(getGlyphColor(glyph))), 0, word
								.length(), 0);
			}
			builder.append(spanWord);
		}
		return builder;
	}

	private static String getGlyphColor(Glyph glyph) {
		switch (glyph.style) {
		case KEYWORD:
			return RED;
		case SYNTAX:
			return BLUE;
		case VAR:
			return null;
		case TYPE:
			return GREEN;
		default:
			return null;
		}
	}

}