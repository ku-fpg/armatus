package edu.kufpg.armatus.activity;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.data.Glyph;
import edu.kufpg.armatus.gesture.OnPinchZoomListener;
import edu.kufpg.armatus.util.StringUtils;
import edu.kufpg.armatus.util.TextDrawable;
import android.graphics.Color;
import android.os.Bundle;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.ImageSpan;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.widget.TextView;

public class ConsoleEntrySelectionActivity extends ConsoleEntryActivity {
	private static final int DEFAULT_TEXT_SIZE = 17;

	private Spannable mSpans;
	private ScaleGestureDetector mScaleGestureDetector;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console_entry_selection_activity);

		final TextView selectableView = (TextView) findViewById(R.id.console_entry_selection_view);
		final OnPinchZoomListener zoomListener = new OnPinchZoomListener(this, DEFAULT_TEXT_SIZE) {
			@Override
			public void onScaleEnd(ScaleGestureDetector detector) {
				for (ImageSpan span : mSpans.getSpans(0, mSpans.length(), ImageSpan.class)) {
					TextDrawable td = (TextDrawable) span.getDrawable();
					td.setTextSize(getIntSize());
					td.setBounds(0, 0, td.getIntrinsicWidth(), td.getIntrinsicHeight());
				}
				selectableView.setText(mSpans);
				super.onScaleEnd(detector);
			}
		};
		mScaleGestureDetector = new ScaleGestureDetector(this, zoomListener);

		if (savedInstanceState == null) {
			ConsoleEntry entry = getEntry();
			String baseText = StringUtils.noFirstLine(StringUtils.noCharWrap(entry.getShortContents().toString()));
			selectableView.setText(baseText);
			int defaultColor = selectableView.getCurrentTextColor();
			Spannable selectableSpans = new SpannableString(selectableView.getText());

			int index = 0;
			for (Glyph glyph : entry.getCommandResponse().getGlyphs()) {
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
				drawable.setTextSize(DEFAULT_TEXT_SIZE);
				drawable.setTypeface(ConsoleActivity.TYPEFACE);
				drawable.setBounds(0, 0, drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight());

				ImageSpan imageSpan = new ImageSpan(drawable, ImageSpan.ALIGN_BASELINE);
				selectableSpans.setSpan(imageSpan, index, index + contents.length(), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);

				index += contents.length();
			}
			mSpans = selectableSpans;
		} else {
			mSpans = (Spannable) savedInstanceState.getCharSequence("selectableContents");
		}
		selectableView.setText(mSpans);
	}

	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putCharSequence("selectableContents", mSpans);
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {
		mScaleGestureDetector.onTouchEvent(event);
		return super.onTouchEvent(event);
	}
}