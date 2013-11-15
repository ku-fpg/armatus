package edu.kufpg.armatus.activity;

import java.util.List;

import com.google.common.collect.Lists;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.data.Glyph;
import edu.kufpg.armatus.radialmenu.RadialMenuSpan;
import edu.kufpg.armatus.util.LongClickableSpan;
import edu.kufpg.armatus.util.StringUtils;
import edu.kufpg.armatus.util.TextDrawable;
import android.graphics.Color;
import android.os.Bundle;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.ImageSpan;
import android.view.View;
import android.widget.TextView;

public class ConsoleSelectEntryActivity extends ConsoleEntryActivity {
	
	private ConsoleEntry mEntry;
	private CharSequence mSelectableContents;
	
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		
		mEntry = getEntry();
		
		setContentView(R.layout.console_selection_entry_activity);
		
		TextView selectableView = (TextView) findViewById(R.id.console_entry_selection_view);
		
		String baseText = StringUtils.noFirstLine(StringUtils.noCharWrap(mEntry.getShortContents().toString()));
		selectableView.setText(baseText);
		
		int defaultColor = selectableView.getCurrentHintTextColor();
		
		if (savedInstanceState == null) {
			Spannable selectableSpans = new SpannableString(selectableView.getText());
			int index = 0;
			for (Glyph glyph : mEntry.getCommandResponse().getGlyphs()) {
				String contents = glyph.getText();
				if (contents.contains("\n") || contents.contains("\t")) {
					String sanitizedContents = contents.replaceAll("[\n\t]", "");
					index += contents.length() - sanitizedContents.length();
					contents = sanitizedContents;
				}
				TextDrawable drawable = new TextDrawable(this);
				drawable.setText(contents);
				String color = glyph.getColor();
				if (color != null) {
					drawable.setTextColor(Color.parseColor(color));
				} else {
					drawable.setTextColor(defaultColor);
				}
				drawable.setTextSize(17f);
				drawable.setTypeface(ConsoleActivity.TYPEFACE);
				drawable.setBounds(0, 0, drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight());

				ImageSpan imageSpan = new ImageSpan(drawable, ImageSpan.ALIGN_BASELINE);
				selectableSpans.setSpan(imageSpan, index, index + contents.length(), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
				
				index += contents.length();
			}
			mSelectableContents = selectableSpans;
			} else {
				mSelectableContents = savedInstanceState.getCharSequence("selectableContents");
			}
		selectableView.setText(mSelectableContents);
		}
	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putCharSequence("selectableContents", mSelectableContents);
	}
}
