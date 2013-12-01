package edu.kufpg.armatus.activity;

import java.util.List;
import java.util.Map;

import android.graphics.Color;
import android.graphics.Point;
import android.graphics.Rect;
import android.os.Bundle;
import android.os.Parcel;
import android.text.Layout;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.TextPaint;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.view.View;
import android.widget.TextView;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Maps;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.data.Crumb;
import edu.kufpg.armatus.data.Glyph;
import edu.kufpg.armatus.gesture.OnPinchZoomListener;
import edu.kufpg.armatus.util.BundleUtils;
import edu.kufpg.armatus.util.StringUtils;

public class ConsoleEntryScopeActivity extends ConsoleEntryActivity {

	private TextView mTextView;
	private ScopeScrollView mScrollView;
	private GlyphScopeSpan mSelectedSpan;
	private Spannable mSpans;
	private Map<GlyphScopeSpan, GlyphScopeSpan> mSpanParentMap = Maps.newHashMap();
	private ListMultimap<GlyphScopeSpan, GlyphScopeSpan> mSpanChildrenMap = ArrayListMultimap.create();
	private ScaleGestureDetector mScaleGestureDetector;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console_entry_scope_activity);

		mScrollView = (ScopeScrollView) findViewById(R.id.console_entry_scope_scroll_view);
		mTextView = (TextView) findViewById(R.id.console_entry_scope_text_view);
		final OnPinchZoomListener zoomListener = new OnPinchZoomListener(this, (int) mTextView.getTextSize()) {
			@Override
			public void onScaleEnd(ScaleGestureDetector detector) {
				mTextView.setTextSize(getIntSize());
				if (mSelectedSpan != null) {
					mTextView.post(new Runnable() {
						@Override
						public void run() {
							mScrollView.clearLines();
							selectSpan(mSpans, mSelectedSpan);
							mScrollView.invalidate();
						}
					});
				}
				super.onScaleEnd(detector);
			}
		};
		mScaleGestureDetector = new ScaleGestureDetector(this, zoomListener);

		if (savedInstanceState == null) {
			final ConsoleEntry entry = getEntry();
			final Spannable spans = StringUtils.charWrap(entry.getCommandResponse().getGlyphText());
			final Map<List<Crumb>, GlyphScopeSpan> crumbsSpanMap = Maps.newHashMap();
			final int length = spans.length();
			int index = 0;
			for (final Glyph glyph : entry.getCommandResponse().getGlyphs()) {
				int endIndex = index + glyph.getText().length() + 1;
				final GlyphScopeSpan span = new GlyphScopeSpan(this, index, Math.min(length, endIndex - 1));

				crumbsSpanMap.put(glyph.getPath(), span);
				if (glyph.hasBindingSite() && crumbsSpanMap.containsKey(glyph.getBindingSite())) {
					GlyphScopeSpan parent = crumbsSpanMap.get(glyph.getBindingSite());
					mSpanParentMap.put(span, parent);
					mSpanChildrenMap.put(parent, span);
				}

				spans.setSpan(span, index, Math.min(length, endIndex - 1), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
				index = endIndex - 1;
			}
			mSpans = spans;
		} else {
			BundleUtils.getParParMap(savedInstanceState, "spanParentMap", mSpanParentMap);
			BundleUtils.getParParMultimap(savedInstanceState, "spanChildrenMap", mSpanChildrenMap);
			mSelectedSpan = savedInstanceState.getParcelable("selectedSpan");
			mSpans = (Spannable) savedInstanceState.getCharSequence("spans");
			int textSize = savedInstanceState.getInt("textSize");

			for (GlyphScopeSpan span : mSpans.getSpans(0, mSpans.length(), GlyphScopeSpan.class)) {
				span.attach(this);
			}
			if (mSelectedSpan != null) {
				mTextView.post(new Runnable() {
					@Override
					public void run() {
						removeHighlight(mSpans);
						selectSpan(mSpans, mSelectedSpan);
						mTextView.setText(mSpans);
					}	
				});
			}
			// Watch out! setTextSize() takes sp, but textSize is in pixels!
			mTextView.setTextSize(textSize / getResources().getDisplayMetrics().scaledDensity);
			zoomListener.setSize(textSize);
		}

		mTextView.setText(mSpans);
		mTextView.setMovementMethod(ScopeMovementMethod.getInstance());
	}

	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		BundleUtils.putParMap(outState, "spanParentMap", mSpanParentMap);
		BundleUtils.putParMultimap(outState, "spanChildrenMap", mSpanChildrenMap);
		outState.putParcelable("selectedSpan", mSelectedSpan);
		outState.putCharSequence("spans", mSpans);
		outState.putInt("textSize", (int) mTextView.getTextSize());
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {
		mScaleGestureDetector.onTouchEvent(event);
		return super.onTouchEvent(event);
	}

	private Rect getSpanCoordinates(Spannable buffer, Object span) {
		// Initialize global value
		Rect parentTextViewRect = new Rect();

		// Initialize values for the computing of clickedText position
		Layout textViewLayout = mTextView.getLayout();

		double startOffsetOfClickedText = buffer.getSpanStart(span);
		double endOffsetOfClickedText = buffer.getSpanEnd(span);
		double startXCoordinatesOfClickedText = textViewLayout.getPrimaryHorizontal((int)startOffsetOfClickedText);
		double endXCoordinatesOfClickedText = textViewLayout.getPrimaryHorizontal((int)endOffsetOfClickedText);


		// Get the rectangle of the clicked text
		int currentLineStartOffset = textViewLayout.getLineForOffset((int)startOffsetOfClickedText);
		int currentLineEndOffset = textViewLayout.getLineForOffset((int)endOffsetOfClickedText);
		boolean keywordIsInMultiLine = currentLineStartOffset != currentLineEndOffset;
		textViewLayout.getLineBounds(currentLineStartOffset, parentTextViewRect);


		// Update the rectangle position to his real position on screen
		int[] parentTextViewLocation = {0,0};
		mTextView.getLocationOnScreen(parentTextViewLocation);

		double parentTextViewTopAndBottomOffset = (
				parentTextViewLocation[1] - 
				mTextView.getScrollY() + 
				mTextView.getCompoundPaddingTop()
				);
		parentTextViewRect.top += parentTextViewTopAndBottomOffset;
		parentTextViewRect.bottom += parentTextViewTopAndBottomOffset;

		// In the case of multi line text, we have to choose what rectangle take
		if (keywordIsInMultiLine) {

			Point size = new Point();
			getWindowManager().getDefaultDisplay().getSize(size);
			int screenHeight = size.y;

			int dyTop = parentTextViewRect.top;
			int dyBottom = screenHeight - parentTextViewRect.bottom;
			boolean onTop = dyTop > dyBottom;

			if (onTop) {
				endXCoordinatesOfClickedText = textViewLayout.getLineRight(currentLineStartOffset);
			} else {
				parentTextViewRect = new Rect();
				textViewLayout.getLineBounds(currentLineEndOffset, parentTextViewRect);
				parentTextViewRect.top += parentTextViewTopAndBottomOffset;
				parentTextViewRect.bottom += parentTextViewTopAndBottomOffset;
				startXCoordinatesOfClickedText = textViewLayout.getLineLeft(currentLineEndOffset);
			}
		}

		parentTextViewRect.left += (
				parentTextViewLocation[0] +
				startXCoordinatesOfClickedText + 
				mTextView.getCompoundPaddingLeft() - 
				mTextView.getScrollX()
				);
		parentTextViewRect.right = (int) (
				parentTextViewRect.left + 
				endXCoordinatesOfClickedText - 
				startXCoordinatesOfClickedText
				);

		return parentTextViewRect;
	}

	private void selectSpan(Spannable textViewSpans, GlyphScopeSpan spanToSelect) {
		Rect coords = getSpanCoordinates(textViewSpans, spanToSelect);
		if (mSpanParentMap.containsKey(spanToSelect)) {
			ScopeSpan ps = mSpanParentMap.get(spanToSelect);
			mScrollView.drawParentalLine(coords, getSpanCoordinates(textViewSpans, ps));

			textViewSpans.setSpan(new HighlightBackgroundSpan(Color.CYAN),
					spanToSelect.getStartIndex(), spanToSelect.getEndIndex(), 0);
			textViewSpans.setSpan(new HighlightTextSpan(Color.BLACK),
					spanToSelect.getStartIndex(), spanToSelect.getEndIndex(), 0);
			textViewSpans.setSpan(new HighlightBackgroundSpan(Color.BLUE),
					ps.getStartIndex(), ps.getEndIndex(), 0);
		} else if (mSpanChildrenMap.containsKey(spanToSelect)) {
			List<GlyphScopeSpan> childSpans = mSpanChildrenMap.get(spanToSelect);
			Rect[] childRects = new Rect[childSpans.size()];
			for (int i = 0; i < childSpans.size(); i++) {
				ScopeSpan cs = childSpans.get(i);
				childRects[i] = getSpanCoordinates(textViewSpans, cs);

				textViewSpans.setSpan(new HighlightBackgroundSpan(Color.CYAN),
						cs.getStartIndex(), cs.getEndIndex(), 0);
				textViewSpans.setSpan(new HighlightTextSpan(Color.BLACK),
						cs.getStartIndex(), cs.getEndIndex(), 0);
			}
			mScrollView.drawChildLines(coords, childRects);

			textViewSpans.setSpan(new HighlightBackgroundSpan(Color.BLUE),
					spanToSelect.getStartIndex(), spanToSelect.getEndIndex(), 0);
		}
		mSelectedSpan = spanToSelect;
	}

	private void removeHighlight(Spannable spanSource) {
		HighlightBackgroundSpan[] hBackSpans = spanSource.getSpans(0, mTextView.length(), HighlightBackgroundSpan.class);
		HighlightTextSpan[] hTextSpans = spanSource.getSpans(0, mTextView.length(), HighlightTextSpan.class);
		for (HighlightBackgroundSpan s : hBackSpans) {
			spanSource.removeSpan(s);
		}
		for (HighlightTextSpan s : hTextSpans) {
			spanSource.removeSpan(s);
		}
	}

	private ScopeScrollView getScrollView() {
		return mScrollView;
	}

	private ScopeSpan getSelectedSpan() {
		return mSelectedSpan;
	}

	private TextView getTextView() {
		return mTextView;
	}

	private void setSelectedSpan(GlyphScopeSpan newSpan) {
		mSelectedSpan = newSpan;
	}

	private static class GlyphScopeSpan extends ScopeSpan {
		private ConsoleEntryScopeActivity mActivity;

		private GlyphScopeSpan(ConsoleEntryScopeActivity activity, int startIndex, int endIndex) {
			super(startIndex, endIndex);
			mActivity = activity;
		}

		@Override
		public void onClick(View widget) {
			final TextView tv = mActivity.getTextView();
			final ScopeScrollView sv = mActivity.getScrollView();
			final ScopeSpan selectedSpan = mActivity.getSelectedSpan();

			Spannable textViewSpans = new SpannableString(tv.getText());
			mActivity.removeHighlight(textViewSpans);
			sv.clearLines();

			if (selectedSpan == null || !selectedSpan.equals(this)) {
				mActivity.selectSpan(textViewSpans, this);
			} else if (selectedSpan != null) {
				mActivity.setSelectedSpan(null);
			}
			sv.requestLayout();
			tv.setText(textViewSpans);
		}

		@Override
		public void updateDrawState(TextPaint ds) {}

		private void attach(ConsoleEntryScopeActivity activity) {
			mActivity = activity;
		}
	}

	private static class HighlightBackgroundSpan extends BackgroundColorSpan {
		public HighlightBackgroundSpan(int color) {
			super(color);
		}

		public HighlightBackgroundSpan(Parcel src) {
			super(src);
		}
	}

	private static class HighlightTextSpan extends ForegroundColorSpan {
		public HighlightTextSpan(int color) {
			super(color);
		}

		public HighlightTextSpan(Parcel src) {
			super(src);
		}

	}

}
