package edu.kufpg.armatus.util;

import android.content.Context;
import android.text.Layout;
import android.text.Spannable;
import android.text.method.LinkMovementMethod;
import android.text.method.MovementMethod;
import android.view.GestureDetector;
import android.view.GestureDetector.SimpleOnGestureListener;
import android.view.MotionEvent;
import android.widget.TextView;

public class LongClickMovementMethod extends LinkMovementMethod {
	private static LongClickMovementMethod sInstance;
	private final GestureDetector mGestureDetector;
	private LongClickableSpan mCurrentSpan;
	private TextView mWidget;

	private LongClickMovementMethod(Context context) {
		super();
		mGestureDetector = new GestureDetector(context, new SimpleOnGestureListener() {
			@Override
			public void onLongPress(MotionEvent e) {
				mCurrentSpan.onLongClick(mWidget);
			}
		});
	}

	@Override
	public boolean onTouchEvent(TextView widget, Spannable buffer, MotionEvent event) {
		int action = event.getAction();

		if (action == MotionEvent.ACTION_UP ||
				action == MotionEvent.ACTION_DOWN) {
			int x = (int) event.getX();
			int y = (int) event.getY();

			x -= widget.getTotalPaddingLeft();
			y -= widget.getTotalPaddingTop();

			x += widget.getScrollX();
			y += widget.getScrollY();

			Layout layout = widget.getLayout();
			int line = layout.getLineForVertical(y);
			int off = layout.getOffsetForHorizontal(line, x);

			LongClickableSpan[] link = buffer.getSpans(off, off, LongClickableSpan.class);

			if (link.length != 0) {
				mCurrentSpan = link[0];
				if (mWidget == null) {
					mWidget = widget;
				}
				mGestureDetector.onTouchEvent(event);
			}
		}

		return super.onTouchEvent(widget, buffer, event);
	}

	public static MovementMethod getInstance(Context context) {
		if (sInstance == null) {
			sInstance = new LongClickMovementMethod(context);
		}

		return sInstance;
	}
}
