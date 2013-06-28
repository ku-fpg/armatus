package com.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import com.kufpg.armatus.R;

import android.graphics.Color;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.BackgroundColorSpan;
import android.text.style.CharacterStyle;
import android.widget.TextView;

public class TextHighlighter {
	private ConsoleActivity mConsole;

	public TextHighlighter(ConsoleActivity console) {
		mConsole = console;
	}

	public void highlightText(String word) {
		unhighlightText();
		if (!word.isEmpty()) {
			for (int i = 0; i < mConsole.getEntryCount(); i++) {
				TextView tv = (TextView) mConsole.getListView().getChildAt(i)
						.findViewById(R.id.console_entry_contents);
				List<Integer> matches = getMatchIndexes(word, tv.getText().toString());
				if (matches != null) {
					Spannable contents = new SpannableString(tv.getText());
					for (int index : matches) {
						CharacterStyle highlight = new BackgroundColorSpan(Color.DKGRAY);
						contents.setSpan(highlight, index, index + word.length(), 0);
					}
					tv.setText(contents);
				}
			}
		}
	}

	public void unhighlightText() {
		for (int i = 0; i < mConsole.getEntryCount(); i++) {
			TextView tv = (TextView) mConsole.getListView().getChildAt(i)
					.findViewById(R.id.console_entry_contents);
			Spannable noHighlight = new SpannableString(tv.getText());
			CharacterStyle[] spans = noHighlight.getSpans(0,
					noHighlight.length(), BackgroundColorSpan.class);
			for (CharacterStyle span : spans) {
				noHighlight.removeSpan(span);
			}
			tv.setText(noHighlight);
		}
	}

	private static List<Integer> getMatchIndexes(String match, String word) {
		String m = match.toLowerCase(Locale.US);
		String w = word.toLowerCase(Locale.US);
		List<Integer> matches = new ArrayList<Integer>();
		for (int i = w.indexOf(m); i >= 0; i = w.indexOf(m, i+1)) {
			matches.add(i);
		}
		if (!matches.isEmpty()) {
			return matches;
		} else {
			return null;
		}
	}

}
