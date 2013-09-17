package edu.kufpg.armatus.radialmenu;

import android.text.Layout;
import android.text.Selection;
import android.text.Spannable;
import android.text.method.LinkMovementMethod;
import android.text.method.MovementMethod;
import android.view.MotionEvent;
import android.widget.TextView;

public class RadialMenuMovementMethod extends LinkMovementMethod {
	private static RadialMenuMovementMethod sInstance;
	private RadialMenuSpanRenderer mRenderer;

	private RadialMenuMovementMethod(RadialMenuSpanRenderer renderer) {
		super();
		mRenderer = renderer;
	}

	@Override
	public boolean onTouchEvent(TextView widget, Spannable buffer,
			MotionEvent event) {
		int action = event.getAction();
		MotionEvent event2 = null;
		
		int x = (int) event.getX();
		int y = (int) event.getY();
		int paddingLeft = widget.getTotalPaddingLeft();
		int paddingTop = widget.getTotalPaddingTop();
		int scrollX = widget.getScrollX();
		int scrollY = widget.getScrollY();

		x -= paddingLeft;
		y -= paddingTop;

		x += scrollX;
		y += scrollY;
		
		event2 = MotionEvent.obtain(event);
		event2.offsetLocation(widget.getX() - paddingLeft + scrollX, widget.getY() - paddingTop + scrollY);

		if (action == MotionEvent.ACTION_UP ||
				action == MotionEvent.ACTION_DOWN) {

			Layout layout = widget.getLayout();
			int line = layout.getLineForVertical(y);
			int off = layout.getOffsetForHorizontal(line, x);

			RadialMenuSpan[] spans = buffer.getSpans(off, off, RadialMenuSpan.class);

			if (spans.length != 0) {
				mRenderer.setRadialMenuContent(spans[0].getMenuContent());
			}
		}

		if (mRenderer.touchMenuView(event2)) {
			Selection.removeSelection(buffer);
		}
		event2.recycle();
		return super.onTouchEvent(widget, buffer, event);
	}

	public static MovementMethod getInstance(RadialMenuSpanRenderer renderer) {
		if (sInstance == null) {
			sInstance = new RadialMenuMovementMethod(renderer);
		}

		return sInstance;
	}

}
