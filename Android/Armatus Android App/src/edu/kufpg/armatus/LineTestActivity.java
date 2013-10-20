package edu.kufpg.armatus;

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
import edu.kufpg.armatus.util.StringUtils;

public class LineTestActivity extends BaseActivity {
	private TextView mTextView;
	private LineTestScrollView mScrollView;
	private LineTestSpan mLinedSpan, mSelectedSpan;

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.line_test_activity);

		mScrollView = (LineTestScrollView) findViewById(R.id.line_test_scroll_view);
		mTextView = (TextView) findViewById(R.id.line_test_text_view);

		mTextView.setText(StringUtils.withCharWrap("This is a test of the scoping capabilities that are soon to be integrated into Armatus " +
				"I hope it works I actually have no idea this could end poorly so we'll see what happens here goes nothing "));
		for (int i = 0; i < 3; i++) {
			mTextView.append(mTextView.getText());
		}

		final Spannable spans = new SpannableString(mTextView.getText());
		final int length = mTextView.length();
		int index = 0;
		for (final String word : mTextView.getText().toString().split(StringUtils.WHITESPACE)) {
			final LineTestSpan span = new LineTestSpan(index, Math.min(length, index + word.length() + 1), null) {
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
			mLinedSpan = span;
			spans.setSpan(span, index, Math.min(length, index + word.length() + 1), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
			index += word.length() + 1;
		}

		for (LineTestSpan span : spans.getSpans(0, spans.length(), LineTestSpan.class)) {
			if (!span.equals(mLinedSpan)) {
				span.addChildSpan(mLinedSpan);
			}
			if (!mLinedSpan.hasParentSpan()) {
				mLinedSpan.setParentSpan(span);
			}
		}

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
		if (keywordIsInMultiLine){

			Point size = new Point();
			getWindowManager().getDefaultDisplay().getSize(size);
			int screenHeight = size.y;

			int dyTop = parentTextViewRect.top;
			int dyBottom = screenHeight - parentTextViewRect.bottom;
			boolean onTop = dyTop > dyBottom;

			if (onTop){
				endXCoordinatesOfClickedText = textViewLayout.getLineRight(currentLineStartOffset);
			}
			else{
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
