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
import android.view.View;
import android.widget.TextView;

import com.google.common.collect.Maps;

import edu.kufpg.armatus.LineTestMovementMethod;
import edu.kufpg.armatus.LineTestScrollView;
import edu.kufpg.armatus.LineTestSpan;
import edu.kufpg.armatus.R;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.data.Crumb;
import edu.kufpg.armatus.data.Glyph;
import edu.kufpg.armatus.util.StringUtils;

public class ConsoleEntryScopeActivity extends ConsoleEntryActivity {

	private TextView mTextView;
	private LineTestScrollView mScrollView;
	private LineTestSpan mSelectedSpan;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.console_entry_scope_activity);

		mScrollView = (LineTestScrollView) findViewById(R.id.console_entry_scope_scroll_view);
		mTextView = (TextView) findViewById(R.id.console_entry_scope_text_view);
		final ConsoleEntry entry = getEntry();

		final Spannable spans = StringUtils.charWrap(entry.getCommandResponse().getGlyphText());
		//		final Map<LineTestSpan, LineTestSpan> mSpanParentMap = Maps.newHashMap();
		//		final ListMultimap<LineTestSpan, LineTestSpan> mSpanChildrenMap = ArrayListMultimap.create();
		final Map<List<Crumb>, LineTestSpan> crumbsSpanMap = Maps.newHashMap();
		final int length = spans.length();
		int index = 0;
		for (final Glyph glyph : entry.getCommandResponse().getGlyphs()) {
			int endIndex = index + glyph.getText().length() + 1;
			final LineTestSpan span = new LineTestSpan(index, Math.min(length, endIndex - 1)) {
				@Override
				public void onClick(View widget) {
					Spannable textViewSpans = new SpannableString(mTextView.getText());
					removeHighlight(textViewSpans);
					mScrollView.clearLines();

					if (mSelectedSpan == null || !mSelectedSpan.equals(this)) {
						selectSpan(textViewSpans, this);
					} else if (mSelectedSpan != null) {
						mSelectedSpan = null;
					}
					mScrollView.requestLayout();
					mTextView.setText(textViewSpans);
				}

				@Override
				public void updateDrawState(TextPaint ds) {}
			};

			crumbsSpanMap.put(glyph.getPath(), span);
			if (glyph.hasBindingSite() && crumbsSpanMap.containsKey(glyph.getBindingSite())) {
				LineTestSpan parent = crumbsSpanMap.get(glyph.getBindingSite());
				span.setParentSpan(parent);
				parent.addChildSpan(span);
			}

			spans.setSpan(span, index, Math.min(length, endIndex - 1), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
			index = endIndex - 1;
		}

		//		for (LineTestSpan span : spans.getSpans(0, spans.length(), LineTestSpan.class)) {
		//			if (!span.equals(mLinedSpan)) {
		//				span.addChildSpan(mLinedSpan);
		//			}
		//			if (!mLinedSpan.hasParentSpan()) {
		//				mLinedSpan.setParentSpan(span);
		//			}
		//		}

		mTextView.setText(spans);
		mTextView.setMovementMethod(LineTestMovementMethod.getInstance());
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

	private void selectSpan(Spannable textViewSpans, LineTestSpan spanToSelect) {
		Rect coords = getSpanCoordinates(textViewSpans, spanToSelect);
		if (spanToSelect.hasParentSpan()) {
			LineTestSpan ps = spanToSelect.getParentSpan();
			mScrollView.drawParentalLine(coords, getSpanCoordinates(textViewSpans, ps));

			textViewSpans.setSpan(new HighlightBackgroundSpan(Color.CYAN),
					spanToSelect.getStartIndex(), spanToSelect.getEndIndex(), 0);
			textViewSpans.setSpan(new HighlightTextSpan(Color.BLACK),
					spanToSelect.getStartIndex(), spanToSelect.getEndIndex(), 0);
			textViewSpans.setSpan(new HighlightBackgroundSpan(Color.BLUE),
					ps.getStartIndex(), ps.getEndIndex(), 0);
		} else if (spanToSelect.hasChildSpans()) {
			Rect[] childRects = new Rect[spanToSelect.getChildSpanCount()];
			for (int i = 0; i < spanToSelect.getChildSpanCount(); i++) {
				LineTestSpan cs = spanToSelect.getChildSpan(i);
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

	private class HighlightBackgroundSpan extends BackgroundColorSpan {

		public HighlightBackgroundSpan(int color) {
			super(color);
		}

		public HighlightBackgroundSpan(Parcel src) {
			super(src);
		}
	}

	private class HighlightTextSpan extends ForegroundColorSpan {

		public HighlightTextSpan(int color) {
			super(color);
		}

		public HighlightTextSpan(Parcel src) {
			super(src);
		}

	}

}
