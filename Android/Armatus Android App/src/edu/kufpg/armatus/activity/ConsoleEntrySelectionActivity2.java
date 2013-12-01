package edu.kufpg.armatus.activity;

import java.util.Map;

import android.os.Bundle;
import android.text.Selection;
import android.text.Spannable;
import android.view.ActionMode;
import android.view.ActionMode.Callback;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.ImageButton;

import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;
import com.google.common.collect.TreeRangeMap;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.activity.SelectionTextView.SelectionWatcher;
import edu.kufpg.armatus.data.Glyph;
import edu.kufpg.armatus.gesture.OnPinchZoomListener;
import edu.kufpg.armatus.util.BundleUtils;
import edu.kufpg.armatus.util.TurboImageButton;

public class ConsoleEntrySelectionActivity2 extends ConsoleEntryActivity {
	private SelectionTextView mTextView;
	private RangeMap<Integer, Glyph> mGlyphRangeMap = TreeRangeMap.create();
	private int mSelStart = -1;
	private int mSelEnd = -1;
	private ScaleGestureDetector mScaleGestureDetector;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console_entry_selection_activity2);

		mTextView = (SelectionTextView) findViewById(R.id.console_entry_selection_view2);
		mTextView.addSelectionWatcher(new SelectionWatcher() {
			@Override
			public void onSelectionChanged(int selStart, int selEnd) {
				mSelStart = selStart;
				mSelEnd = selEnd;
			}
		});
		mTextView.setCustomSelectionActionModeCallback(new GlyphSelectionCallback());
		mTextView.setText(getEntry().getCommandResponse().getGlyphText());
		final OnPinchZoomListener zoomListener = new OnPinchZoomListener(this, (int) mTextView.getTextSize()) {
			@Override
			public void onScaleEnd(ScaleGestureDetector detector) {
				final int tempSelStart = mSelStart;
				final int tempSelEnd = mSelEnd;

				mTextView.setTextSize(getIntSize());
				mTextView.post(new Runnable() {
					@Override
					public void run() {
						Selection.setSelection((Spannable) mTextView.getText(), tempSelStart, tempSelEnd);
						mTextView.performLongClick();
					}
				});
				super.onScaleEnd(detector);
			}
		};
		mScaleGestureDetector = new ScaleGestureDetector(this, zoomListener);

		if (savedInstanceState == null) {
			int index = 0;
			for (Glyph glyph : getEntry().getCommandResponse().getGlyphs()) {
				if (!glyph.getText().isEmpty()) {
					Range<Integer> glyphRange = Range.closedOpen(index, index + glyph.getText().length());
					mGlyphRangeMap.put(glyphRange, glyph);
					index += glyph.getText().length();
				}
			}
		} else {
			BundleUtils.getIntParRangeMap(savedInstanceState, "glyphRangeMap", mGlyphRangeMap);
		}
	}

	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		BundleUtils.putIntParRangeMap(outState, "glyphRangeMap", mGlyphRangeMap);
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {
		mScaleGestureDetector.onTouchEvent(event);
		return super.onTouchEvent(event);
	}

	private class GlyphSelectionCallback implements Callback {

		@Override
		public boolean onCreateActionMode(ActionMode mode, Menu menu) {
			MenuInflater inflater = mode.getMenuInflater();
			inflater.inflate(R.menu.console_entry_selection_action_mode, menu);
			menu.removeItem(android.R.id.selectAll);
			menu.removeItem(android.R.id.copy);
			mode.setTitle("Text selection");

			View actionView = menu.findItem(R.id.console_entry_selection_glyph_navigation).getActionView();
			ImageButton prevGlyph = (TurboImageButton) actionView.findViewById(R.id.test123);
			ImageButton wrapGlyphs = (ImageButton) actionView.findViewById(R.id.test456);
			ImageButton nextGlyph = (TurboImageButton) actionView.findViewById(R.id.test789);

			prevGlyph.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View v) {
					Map.Entry<Range<Integer>, Glyph> entry = mGlyphRangeMap.getEntry(mSelStart - 1);
					if (entry != null) {
						mSelStart = entry.getKey().lowerEndpoint();
						Selection.setSelection((Spannable) mTextView.getText(), mSelStart, mSelEnd);
					}
				}
			});
			wrapGlyphs.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View v) {
					mSelStart = mGlyphRangeMap.getEntry(mSelStart).getKey().lowerEndpoint();
					mSelEnd = mGlyphRangeMap.getEntry(mSelEnd - 1).getKey().upperEndpoint();
					Selection.setSelection((Spannable) mTextView.getText(), mSelStart, mSelEnd);
				}
			});
			nextGlyph.setOnClickListener(new OnClickListener() {
				@Override
				public void onClick(View v) {
					Map.Entry<Range<Integer>, Glyph> entry = mGlyphRangeMap.getEntry(mSelEnd);
					if (entry != null) {
						mSelEnd = entry.getKey().upperEndpoint();
						Selection.setSelection((Spannable) mTextView.getText(), mSelStart, mSelEnd);
					}
				}
			});
			return true;
		}

		@Override
		public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
			return true;
		}

		@Override
		public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
			return false;
		}

		@Override
		public void onDestroyActionMode(ActionMode mode) {}
	}

}
