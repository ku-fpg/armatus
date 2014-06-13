package edu.kufpg.armatus.activity;

import java.util.List;
import java.util.Map;

import android.graphics.Color;
import android.graphics.Point;
import android.graphics.Rect;
import android.os.Bundle;
import android.os.Parcel;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.text.Layout;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.Spanned;
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

    @Override public void onCreate(@Nullable final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.console_entry_scope_activity);

        mScrollView = (ScopeScrollView) findViewById(R.id.console_entry_scope_scroll_view);
        mTextView = (TextView) findViewById(R.id.console_entry_scope_text_view);
        final OnPinchZoomListener zoomListener = new OnPinchZoomListener(this, (int) mTextView.getTextSize()) {
            @Override public void onScaleEnd(@NonNull final ScaleGestureDetector detector) {
                mTextView.setTextSize(getIntSize());
                if (mSelectedSpan != null) {
                    mTextView.post(new Runnable() {
                        @Override public void run() {
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
                final int endIndex = index + glyph.getText().length() + 1;
                final GlyphScopeSpan span = new GlyphScopeSpan(this, index, Math.min(length, endIndex - 1));

                crumbsSpanMap.put(glyph.getPath(), span);
                if (glyph.hasBindingSite() && crumbsSpanMap.containsKey(glyph.getBindingSite())) {
                    final GlyphScopeSpan parent = crumbsSpanMap.get(glyph.getBindingSite());
                    mSpanParentMap.put(span, parent);
                    mSpanChildrenMap.put(parent, span);
                }

                spans.setSpan(span, index, Math.min(length, endIndex - 1), Spanned.SPAN_INCLUSIVE_EXCLUSIVE);
                index = endIndex - 1;
            }
            mSpans = spans;
        } else {
            mSpanParentMap = BundleUtils.getMap(savedInstanceState, "spanParentMap");
            mSpanChildrenMap = BundleUtils.getListMultimap(savedInstanceState, "spanChildrenMap");
            mSelectedSpan = savedInstanceState.getParcelable("selectedSpan");
            mSpans = (Spannable) savedInstanceState.getCharSequence("spans");
            final int textSize = savedInstanceState.getInt("textSize");

            for (final GlyphScopeSpan span : mSpans.getSpans(0, mSpans.length(), GlyphScopeSpan.class)) {
                span.attach(this);
            }
            if (mSelectedSpan != null) {
                mTextView.post(new Runnable() {
                    @Override public void run() {
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

    @Override public void onSaveInstanceState(@NonNull final Bundle outState) {
        super.onSaveInstanceState(outState);
        BundleUtils.putMap(outState, "spanParentMap", mSpanParentMap);
        BundleUtils.putMultimap(outState, "spanChildrenMap", mSpanChildrenMap);
        outState.putParcelable("selectedSpan", mSelectedSpan);
        outState.putCharSequence("spans", mSpans);
        outState.putInt("textSize", (int) mTextView.getTextSize());
    }

    @Override public boolean onTouchEvent(@NonNull final MotionEvent event) {
        mScaleGestureDetector.onTouchEvent(event);
        return super.onTouchEvent(event);
    }

    @NonNull private Rect getSpanCoordinates(@NonNull final Spannable buffer,
                                             @NonNull final Object span) {
        // Initialize global value
        Rect parentTextViewRect = new Rect();

        // Initialize values for the computing of clickedText position
        final Layout textViewLayout = mTextView.getLayout();

        final double startOffsetOfClickedText = buffer.getSpanStart(span);
        final double endOffsetOfClickedText = buffer.getSpanEnd(span);
        double startXCoordinatesOfClickedText = textViewLayout.getPrimaryHorizontal((int) startOffsetOfClickedText);
        double endXCoordinatesOfClickedText = textViewLayout.getPrimaryHorizontal((int) endOffsetOfClickedText);


        // Get the rectangle of the clicked text
        final int currentLineStartOffset = textViewLayout.getLineForOffset((int) startOffsetOfClickedText);
        final int currentLineEndOffset = textViewLayout.getLineForOffset((int) endOffsetOfClickedText);
        final boolean keywordIsInMultiLine = currentLineStartOffset != currentLineEndOffset;
        textViewLayout.getLineBounds(currentLineStartOffset, parentTextViewRect);


        // Update the rectangle position to his real position on screen
        final int[] parentTextViewLocation = {0, 0};
        mTextView.getLocationOnScreen(parentTextViewLocation);

        final double parentTextViewTopAndBottomOffset = parentTextViewLocation[1] -
                mTextView.getScrollY() +
                mTextView.getCompoundPaddingTop();
        parentTextViewRect.top += (int) parentTextViewTopAndBottomOffset;
        parentTextViewRect.bottom += (int) parentTextViewTopAndBottomOffset;

        // In the case of multi line text, we have to choose what rectangle take
        if (keywordIsInMultiLine) {

            final Point size = new Point();
            getWindowManager().getDefaultDisplay().getSize(size);
            final int screenHeight = size.y;

            final int dyTop = parentTextViewRect.top;
            final int dyBottom = screenHeight - parentTextViewRect.bottom;
            final boolean onTop = dyTop > dyBottom;

            if (onTop) {
                endXCoordinatesOfClickedText = textViewLayout.getLineRight(currentLineStartOffset);
            } else {
                parentTextViewRect = new Rect();
                textViewLayout.getLineBounds(currentLineEndOffset, parentTextViewRect);
                parentTextViewRect.top += (int) parentTextViewTopAndBottomOffset;
                parentTextViewRect.bottom += (int) parentTextViewTopAndBottomOffset;
                startXCoordinatesOfClickedText = textViewLayout.getLineLeft(currentLineEndOffset);
            }
        }

        parentTextViewRect.left += (int) (parentTextViewLocation[0] +
                startXCoordinatesOfClickedText +
                mTextView.getCompoundPaddingLeft() -
                mTextView.getScrollX());
        parentTextViewRect.right = (int) (parentTextViewRect.left +
                endXCoordinatesOfClickedText -
                startXCoordinatesOfClickedText);

        return parentTextViewRect;
    }

    private void selectSpan(@NonNull final Spannable textViewSpans,
                            @NonNull final GlyphScopeSpan spanToSelect) {
        final Rect coords = getSpanCoordinates(textViewSpans, spanToSelect);
        if (mSpanParentMap.containsKey(spanToSelect)) {
            final ScopeSpan ps = mSpanParentMap.get(spanToSelect);
            mScrollView.drawParentalLine(coords, getSpanCoordinates(textViewSpans, ps));

            textViewSpans.setSpan(new HighlightBackgroundSpan(Color.CYAN),
                    spanToSelect.getStartIndex(), spanToSelect.getEndIndex(), 0);
            textViewSpans.setSpan(new HighlightTextSpan(Color.BLACK),
                    spanToSelect.getStartIndex(), spanToSelect.getEndIndex(), 0);
            textViewSpans.setSpan(new HighlightBackgroundSpan(Color.BLUE),
                    ps.getStartIndex(), ps.getEndIndex(), 0);
        } else if (mSpanChildrenMap.containsKey(spanToSelect)) {
            final List<GlyphScopeSpan> childSpans = mSpanChildrenMap.get(spanToSelect);
            final Rect[] childRects = new Rect[childSpans.size()];
            for (int i = 0; i < childSpans.size(); i++) {
                final ScopeSpan cs = childSpans.get(i);
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

    private void removeHighlight(@NonNull final Spannable spanSource) {
        final HighlightBackgroundSpan[] hBackSpans = spanSource.getSpans(0, mTextView.length(), HighlightBackgroundSpan.class);
        final HighlightTextSpan[] hTextSpans = spanSource.getSpans(0, mTextView.length(), HighlightTextSpan.class);
        for (final HighlightBackgroundSpan s : hBackSpans) {
            spanSource.removeSpan(s);
        }
        for (final HighlightTextSpan s : hTextSpans) {
            spanSource.removeSpan(s);
        }
    }

    private static class GlyphScopeSpan extends ScopeSpan {
        private ConsoleEntryScopeActivity mActivity;

        private GlyphScopeSpan(final ConsoleEntryScopeActivity activity, final int startIndex, final int endIndex) {
            super(startIndex, endIndex);
            mActivity = activity;
        }

        @Override public void onClick(@NonNull final View widget) {
            final TextView tv = mActivity.mTextView;
            final ScopeScrollView sv = mActivity.mScrollView;
            final ScopeSpan selectedSpan = mActivity.mSelectedSpan;

            final Spannable textViewSpans = new SpannableString(tv.getText());
            mActivity.removeHighlight(textViewSpans);
            sv.clearLines();

            if (selectedSpan == null || !selectedSpan.equals(this)) {
                mActivity.selectSpan(textViewSpans, this);
            } else {
                setSelectedSpan(null);
            }
            sv.requestLayout();
            tv.setText(textViewSpans);
        }

        @Override public void updateDrawState(@NonNull final TextPaint ds) {}

        private void attach(@NonNull final ConsoleEntryScopeActivity activity) {
            mActivity = activity;
        }

        private void setSelectedSpan(@Nullable final GlyphScopeSpan newSpan) {
            mActivity.mSelectedSpan = newSpan;
        }
    }

    private static class HighlightBackgroundSpan extends BackgroundColorSpan {
        public HighlightBackgroundSpan(final int color) {
            super(color);
        }

        public HighlightBackgroundSpan(final Parcel src) {
            super(src);
        }
    }

    private static class HighlightTextSpan extends ForegroundColorSpan {
        public HighlightTextSpan(final int color) {
            super(color);
        }

        public HighlightTextSpan(final Parcel src) {
            super(src);
        }

    }

}
