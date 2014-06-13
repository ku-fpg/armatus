package edu.kufpg.armatus.activity;

import java.util.List;
import java.util.Map;

import android.support.annotation.NonNull;
import org.apache.commons.collections4.Trie;
import org.apache.commons.collections4.trie.PatriciaTrie;

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

import com.google.common.base.Strings;
import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;
import com.google.common.collect.TreeRangeMap;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.activity.SelectionTextView.SelectionWatcher;
import edu.kufpg.armatus.data.Crumb;
import edu.kufpg.armatus.data.Glyph;
import edu.kufpg.armatus.gesture.OnPinchZoomListener;
import edu.kufpg.armatus.util.TurboImageButton;

public class ConsoleEntrySelectionActivity2 extends ConsoleEntryActivity {
    private static final String DELIMETER = "×"; // Uncommon character

    private SelectionTextView mTextView;
    private final RangeMap<Integer, Glyph> mRangeGlyphMap = TreeRangeMap.create();
    private int mSelStart = -1;
    private int mSelEnd = -1;
    private ScaleGestureDetector mScaleGestureDetector;
    private final Trie<String, Range<Integer>> mGlyphPathTrie = new PatriciaTrie<Range<Integer>>();

    @NonNull private static String pathToString(@NonNull final List<Crumb> path) {
        final StringBuilder pathStrBuilder = new StringBuilder("");
        if (!path.isEmpty()) {
            pathStrBuilder.append(path.get(0));
            final int size = path.size();
            for (int i = 1; i < size; i++) {
                pathStrBuilder.append(DELIMETER);
                pathStrBuilder.append(path.get(i));
            }
        }
        return pathStrBuilder.toString();
    }

    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.console_entry_selection_activity2);

        mTextView = (SelectionTextView) findViewById(R.id.console_entry_selection_view2);
        mTextView.addSelectionWatcher(new SelectionWatcher() {
            @Override public void onSelectionChanged(final int selStart, final int selEnd) {
                mSelStart = selStart;
                mSelEnd = selEnd;
            }
        });
        mTextView.setCustomSelectionActionModeCallback(new GlyphSelectionCallback());
        mTextView.setText(getEntry().getCommandResponse().getGlyphText());
        final OnPinchZoomListener zoomListener = new OnPinchZoomListener(this, (int) mTextView.getTextSize()) {
            @Override public void onScaleEnd(@NonNull final ScaleGestureDetector detector) {
                final int tempSelStart = mSelStart;
                final int tempSelEnd = mSelEnd;

                mTextView.setTextSize(getIntSize());
                mTextView.post(new Runnable() {
                    @Override public void run() {
                        Selection.setSelection((Spannable) mTextView.getText(), tempSelStart, tempSelEnd);
                        mTextView.performLongClick();
                    }
                });
                super.onScaleEnd(detector);
            }
        };
        mScaleGestureDetector = new ScaleGestureDetector(this, zoomListener);

        // RangeMap/Trie initialization
        int index = 0;
        for (final Glyph glyph : getEntry().getCommandResponse().getGlyphs()) {
            if (!glyph.getText().isEmpty()) {
                final Range<Integer> glyphRange = Range.closedOpen(index, index + glyph.getText().length());
                mRangeGlyphMap.put(glyphRange, glyph);

                final String pathStr = pathToString(glyph.getPath());
                mGlyphPathTrie.put(pathStr, glyphRange);

                index += glyph.getText().length();
            }
        }
    }

    @Override public boolean onTouchEvent(@NonNull final MotionEvent event) {
        mScaleGestureDetector.onTouchEvent(event);
        return super.onTouchEvent(event);
    }

    private class GlyphSelectionCallback implements Callback {
        private TurboImageButton mPrevGlyph, mNextGlyph, mParentGlyph;

        @Override public boolean onCreateActionMode(@NonNull final ActionMode mode,
                                                    @NonNull final Menu menu) {
            final MenuInflater inflater = mode.getMenuInflater();
            inflater.inflate(R.menu.console_entry_selection_action_mode, menu);
            menu.removeItem(android.R.id.selectAll);
            menu.removeItem(android.R.id.copy);
            mode.setTitle("Text selection");

            final View actionView = menu.findItem(R.id.console_entry_selection_glyph_navigation).getActionView();
            mPrevGlyph = (TurboImageButton) actionView.findViewById(R.id.console_entry_selection_prev_glyph);
            mPrevGlyph.enableTurbo();
            final ImageButton wrapGlyphs = (ImageButton) actionView.findViewById(R.id.console_entry_selection_wrap_glyphs);
            mNextGlyph = (TurboImageButton) actionView.findViewById(R.id.console_entry_selection_next_glyph);
            mNextGlyph.enableTurbo();
            mParentGlyph = (TurboImageButton) actionView.findViewById(R.id.console_entry_selection_parent_glyph);
            mParentGlyph.enableTurbo();

            mPrevGlyph.setOnClickListener(new OnClickListener() {
                @Override public void onClick(@NonNull final View v) {
                    final Map.Entry<Range<Integer>, Glyph> entry = mRangeGlyphMap.getEntry(mSelStart - 1);
                    if (entry != null) {
                        mSelStart = entry.getKey().lowerEndpoint();
                        Selection.setSelection((Spannable) mTextView.getText(), mSelStart, mSelEnd);
                    }
                }
            });
            wrapGlyphs.setOnClickListener(new OnClickListener() {
                @Override public void onClick(@NonNull final View v) {
                    mSelStart = mRangeGlyphMap.getEntry(mSelStart).getKey().lowerEndpoint();
                    mSelEnd = mRangeGlyphMap.getEntry(mSelEnd - 1).getKey().upperEndpoint();
                    Selection.setSelection((Spannable) mTextView.getText(), mSelStart, mSelEnd);
                }
            });
            mNextGlyph.setOnClickListener(new OnClickListener() {
                @Override public void onClick(@NonNull final View v) {
                    final Map.Entry<Range<Integer>, Glyph> entry = mRangeGlyphMap.getEntry(mSelEnd);
                    if (entry != null) {
                        mSelEnd = entry.getKey().upperEndpoint();
                        Selection.setSelection((Spannable) mTextView.getText(), mSelStart, mSelEnd);
                    }
                }
            });
            mParentGlyph.setOnClickListener(new OnClickListener() {
                @Override public void onClick(@NonNull final View v) {
                    final RangeMap<Integer, Glyph> subMap = mRangeGlyphMap.subRangeMap(Range.closedOpen(mSelStart, mSelEnd));
                    String prefix = "";
                    for (final Glyph glyph : subMap.asMapOfRanges().values()) {
                        if (!glyph.getPath().isEmpty()) {
                            if (prefix.isEmpty()) {
                                prefix = pathToString(glyph.getPath());
                            } else {
                                prefix = Strings.commonPrefix(prefix, pathToString(glyph.getPath()));
                            }
                        }
                    }

                    final int index = prefix.lastIndexOf(DELIMETER);
                    if (index == -1) {
                        mSelStart = 0;
                        mSelEnd = mTextView.length();
                    } else {
                        final String parentPrefix = prefix.substring(0, index);
                        Range<Integer> parentRange = Range.singleton(mSelStart);
                        for (final Range<Integer> range : mGlyphPathTrie.prefixMap(parentPrefix).values()) {
                            parentRange = parentRange.span(range);
                        }
                        mSelStart = parentRange.lowerEndpoint();
                        mSelEnd = parentRange.upperEndpoint();
                    }
                    Selection.setSelection((Spannable) mTextView.getText(), mSelStart, mSelEnd);
                }
            });
            return true;
        }

        @Override public boolean onPrepareActionMode(@NonNull final ActionMode mode,
                                                     @NonNull final Menu menu) {
            return true;
        }

        @Override public boolean onActionItemClicked(@NonNull final ActionMode mode,
                                                     @NonNull final MenuItem item) {
            return false;
        }

        @Override public void onDestroyActionMode(@NonNull final ActionMode mode) {
            mPrevGlyph.disableTurbo();
            mNextGlyph.disableTurbo();
        }
    }

}
