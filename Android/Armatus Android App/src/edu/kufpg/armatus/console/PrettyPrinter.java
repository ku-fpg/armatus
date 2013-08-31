package edu.kufpg.armatus.console;

import java.util.List;

import edu.kufpg.armatus.command.CustomCommandDispatcher;
import edu.kufpg.armatus.console.HermitClient.Glyph;
import edu.kufpg.armatus.console.HermitClient.GlyphStyle;
import android.graphics.Color;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;

/**
 * Colors text depending on its usage of {@link CustomCommandDispatcher.Keyword
 * Keywords}.
 */
public class PrettyPrinter {
	public static final String BLUE = "#0090D3";
	public static final String RED = "#CC060B";
	public static final String YELLOW = "#FDFD0D";
	public static final String GREEN = "#1DDA1C";
	public static final String CYAN = "#1BE0CC";

	public static CharSequence createPrettyText(List<Glyph> glyphs) {
		SpannableStringBuilder builder = new SpannableStringBuilder();
		for (Glyph glyph : glyphs) {

			SpannableString spanWord = new SpannableString(glyph.text);
			if (glyph.style.equals(GlyphStyle.WARNING)) {
				spanWord.setSpan(new BackgroundColorSpan(Color.YELLOW),
						0, glyph.text.length(), 0);
				spanWord.setSpan(new ForegroundColorSpan(Color.BLACK),
						0, glyph.text.length(), 0);
			} else {
				String glyphColor = getGlyphColor(glyph.style);
				if (glyphColor != null) {
					spanWord.setSpan(new ForegroundColorSpan(Color.parseColor(glyphColor)),
							0, glyph.text.length(), 0);
			}
				
			}
			builder.append(spanWord);
		}
		return builder;
	}

	private static String getGlyphColor(GlyphStyle style) {
		switch (style) {
		case KEYWORD:
			return BLUE;
		case SYNTAX:
			return RED;
		case COERCION:
			return YELLOW;
		case TYPE:
			return GREEN;
		case LIT:
			return CYAN;
		default:
			return null;
		}
	}

}