package edu.kufpg.armatus.console;

import java.util.List;

import edu.kufpg.armatus.command.CommandDispatcher;
import edu.kufpg.armatus.networking.HermitClient.Glyph;
import edu.kufpg.armatus.networking.HermitClient.GlyphStyle;
import edu.kufpg.armatus.util.StringUtils;
import android.graphics.Color;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.ForegroundColorSpan;
import android.widget.TextView;

/**
 * Colors text depending on its usage of {@link CommandDispatcher.Keyword Keywords}.
 */
public class PrettyPrinter {
	public static final String RED = "#CC060B";
	public static final String GREEN = "#1DDA1C";
	public static final String BLUE = "#0090D3";
	public static final String GRAY = "#969696";
	public static final String PURPLE = "#7300FB";
	public static final String YELLOW = "#FDFD0D";

	/**
	 * Sets the text in a {@link TextView} to a string whose {@link
	 * CommandDispatcher.Keyword Keywords} have been colored appropriately.
	 * @param tv The {@code TextView} whose text should be set.
	 * @param text The string to use as the text.
	 */
	public static void setPrettyText(TextView tv, String text) {
		String sentenceStr = text.trim();
		String leadSpace = StringUtils.getLeadingWhitespace(text);
		String trailSpace = StringUtils.getTrailingWhitespace(text);
		String[] sentenceArr = sentenceStr.split(StringUtils.WHITESPACE);
		SpannableStringBuilder builder = new SpannableStringBuilder(leadSpace);
		
		int index = 0;
		for (String word : sentenceArr) {
			index += word.length();
			String followingSpace = StringUtils.getLeadingWhitespace(sentenceStr.substring(index));
			index += followingSpace.length();
			if (CommandDispatcher.isKeyword(word)) {
				SpannableString spanWord = new SpannableString(word + followingSpace);
				spanWord.setSpan(new ForegroundColorSpan(Color.parseColor(CommandDispatcher.
						getKeyword(word).getColor())), 0, word.length(), 0);
				builder.append(spanWord);
			} else {
				builder.append(word + followingSpace);
			}
		}
		
		tv.setText(builder.append(trailSpace));
	}
	
	public static CharSequence createPrettyText(List<Glyph> glyphs)
	{
		SpannableStringBuilder builder = new SpannableStringBuilder();
		for(Glyph glyph : glyphs)
		{
			String word;
			if(!glyph.text.equals("\n"))
			{
				word = glyph.text+StringUtils.NBSP;
			}
			else
			{
				word = glyph.text;
			}
			
			SpannableString spanWord = new SpannableString(word);
			if(!glyph.style.equals(GlyphStyle.NORMAL))
			{
				spanWord.setSpan(new ForegroundColorSpan(Color.parseColor(getGlyphColor(glyph))), 0, word.length(), 0);
			}
			builder.append(spanWord);
		}
		return builder;
	}
	
	private static String getGlyphColor(Glyph glyph)
	{
		switch(glyph.style)
		{
			case KEYWORD:
				return RED;
			case SYNTAX:
				return BLUE;
			case VAR:
				return GREEN;
			case TYPE:
				return PURPLE;
			case LIT:
				return GRAY;
		}
		return null;
	}

}