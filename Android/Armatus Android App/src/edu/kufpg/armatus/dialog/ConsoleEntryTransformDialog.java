package edu.kufpg.armatus.dialog;

import java.util.List;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Lists;
import com.touchmenotapps.widget.radialmenu.menu.v2.RadialMenuItem;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.data.Glyph;
import edu.kufpg.armatus.radialmenu.RadialMenuMovementMethod;
import edu.kufpg.armatus.radialmenu.RadialMenuSpan;
import edu.kufpg.armatus.radialmenu.RadialMenuSpanRenderer;
import edu.kufpg.armatus.util.LongClickMovementMethod;
import edu.kufpg.armatus.util.LongClickableSpan;
import edu.kufpg.armatus.util.StringUtils;
import edu.kufpg.armatus.util.TextDrawable;

import android.graphics.Color;
import android.os.Bundle;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.ImageSpan;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.Window;
import android.widget.FrameLayout;
import android.widget.TabHost;
import android.widget.TabHost.TabContentFactory;
import android.widget.TextView;

/**
 * ConsoleEntrySelectionDialog Class. This class extends {@link android.app.DialogFragment DialogFragment} class. 
 * Task of building strings and making the dialog of the console. 
 */
public class ConsoleEntryTransformDialog extends ConsiderateDialog {
	private ConsoleEntry mEntry;
	private CharSequence mSelectableContents, mLongClickableContents, mRadialClickContents;

	public static ConsoleEntryTransformDialog newInstance(ConsoleEntry entry) {
		ConsoleEntryTransformDialog cetd = new ConsoleEntryTransformDialog();

		Bundle args = new Bundle();
		args.putParcelable("entry", entry);
		cetd.setArguments(args);

		return cetd;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);

		mEntry = getArguments().getParcelable("entry");
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		final View v = inflater.inflate(R.layout.console_entry_transform_dialog, container, false);
		setCancelable(true);
		getDialog().requestWindowFeature(Window.FEATURE_NO_TITLE);
		getDialog().setContentView(R.layout.console_entry_transform_dialog);
		getDialog().setTitle("Entry number " + mEntry.getEntryNum());

		TabHost tabHost = (TabHost) v.findViewById(android.R.id.tabhost);
		tabHost.setup();
		TextView selectableView = (TextView) v.findViewById(R.id.console_entry_transform_selection_view);
		TextView longClickableView = (TextView) v.findViewById(R.id.console_entry_transform_longclick_view);
		TextView radialClickView = (TextView) v.findViewById(R.id.console_entry_transform_radial_view);
		
		FrameLayout parent = (FrameLayout) v.findViewById(android.R.id.tabcontent);
		RadialMenuSpanRenderer renderer = new RadialMenuSpanRenderer(parent, true, 40, 100);
		ListMultimap<String, RadialMenuItem> spanItemsMap = ArrayListMultimap.create();
		RadialMenuItem nothing = new RadialMenuItem("Nothing", "Nothing");
		parent.addView(renderer.renderView());

		String baseText = StringUtils.noFirstLine(StringUtils.noCharWrap(mEntry.getShortContents().toString()));
		selectableView.setText(baseText);
		longClickableView.setText(baseText);
		radialClickView.setText(baseText);

		int defaultColor = selectableView.getCurrentHintTextColor();

		if (savedInstanceState == null) {
			Spannable selectableSpans = new SpannableString(selectableView.getText());
			Spannable longClickableSpans = new SpannableString(longClickableView.getText());
			Spannable radialClickSpans = new SpannableString(radialClickView.getText());
			int index = 0;
			for (Glyph glyph : mEntry.getCommandResponse().getGlyphs()) {
				String contents = glyph.getText();
				if (contents.contains("\n") || contents.contains("\t")) {
					String sanitizedContents = contents.replaceAll("[\n\t]", "");
					index += contents.length() - sanitizedContents.length();
					contents = sanitizedContents;
				}
				TextDrawable drawable = new TextDrawable(getActivity());
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
				longClickableSpans.setSpan(imageSpan, index, index + contents.length(), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
				radialClickSpans.setSpan(imageSpan, index, index + contents.length(), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);

				final String fContents = contents;
				LongClickableSpan longClickSpan = new LongClickableSpan() {
					@Override
					public void onLongClick(View widget) {
						getConsole().showToast("Contents: " + fContents);
					}
				};
				longClickableSpans.setSpan(longClickSpan, index, index + contents.length(), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
				
				List<RadialMenuItem> spanItems;
				if (spanItemsMap.containsKey(contents)) {
					spanItems = spanItemsMap.get(contents);
				} else {
					spanItems = Lists.newArrayList(nothing);
					RadialMenuItem contentsItem = new RadialMenuItem(contents, contents);
					spanItems.add(0, contentsItem);
					spanItemsMap.putAll(contents, spanItems);
				}
				RadialMenuSpan radialMenuSpan = new RadialMenuSpan(spanItems);
				radialClickSpans.setSpan(radialMenuSpan, index, index + contents.length(), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
				
				index += contents.length();
			}
			mSelectableContents = selectableSpans;
			mLongClickableContents = longClickableSpans;
			mRadialClickContents = radialClickSpans;
		} else {
			mSelectableContents = savedInstanceState.getCharSequence("selectableContents");
			mLongClickableContents = savedInstanceState.getCharSequence("longClickableContents");
			mRadialClickContents = savedInstanceState.getCharSequence("radialClickContents");
		}

		selectableView.setText(mSelectableContents);
		longClickableView.setText(mLongClickableContents);
		longClickableView.setMovementMethod(LongClickMovementMethod.getInstance(getActivity()));
		radialClickView.setText(mRadialClickContents);
		radialClickView.setMovementMethod(RadialMenuMovementMethod.getInstance(renderer));

		tabHost.addTab(tabHost.newTabSpec("Selection")
				.setIndicator("Selection")
				.setContent(new TabContentFactory() {
					@Override
					public View createTabContent(String tag) {
						return v.findViewById(R.id.console_entry_transform_selection_container);
					}
				}));
		tabHost.addTab(tabHost.newTabSpec("Long click")
				.setIndicator("Long click")
				.setContent(new TabContentFactory() {
					@Override
					public View createTabContent(String tag) {
						return v.findViewById(R.id.console_entry_transform_longclick_container);
					}
				}));
		tabHost.addTab(tabHost.newTabSpec("Radial menu")
				.setIndicator("Radial menu")
				.setContent(new TabContentFactory() {
					@Override
					public View createTabContent(String tag) {
						return v.findViewById(R.id.console_entry_transform_radial_container);
					}
				}));
		tabHost.setCurrentTabByTag("Radial menu");
		tabHost.setCurrentTabByTag("Long click");
		tabHost.setCurrentTabByTag("Selection");

		return v;
	}

	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putCharSequence("selectableContents", mSelectableContents);
		outState.putCharSequence("longClickableContents", mLongClickableContents);
		outState.putCharSequence("radialClickContents", mRadialClickContents);
	}


}