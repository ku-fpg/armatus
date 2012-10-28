/**
 * MultiTouchView.java
 * 
 * Adapted from MultiTouchVisualizerView.java by Luke Hutchison
 * 
 * (c) Luke Hutchison (luke.hutch@mit.edu)
 * 
 * --
 * 
 * Released under the MIT license (but please notify me if you use this code, so that I can give your project credit at
 * http://code.google.com/p/android-multitouch-controller ).
 * 
 * MIT license: http://www.opensource.org/licenses/MIT
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 */
package com.kufpg.androidhermit.multitouch;

import com.kufpg.androidhermit.multitouch.MultiTouchController.MultiTouchObjectCanvas;
import com.kufpg.androidhermit.multitouch.MultiTouchController.PointInfo;
import com.kufpg.androidhermit.multitouch.MultiTouchController.PositionAndScale;

import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Paint.Style;
import android.util.AttributeSet;
import android.view.MotionEvent;
import android.view.View;

public class MultiTouchView extends View implements MultiTouchObjectCanvas<Object> {

	private MultiTouchController<Object> multiTouchController;
	private PointInfo mCurrTouchPoint;
	private Paint mLinePaintCoords = new Paint(), mRectPaintCoords = new Paint();

	public MultiTouchView(Context context) {
		this(context, null);
	}

	public MultiTouchView(Context context, AttributeSet attrs) {
		this(context, attrs, 0);
	}

	public MultiTouchView(Context context, AttributeSet attrs, int defStyle) {
		super(context, attrs, defStyle);

		multiTouchController = new MultiTouchController<Object>(this);
		mCurrTouchPoint = new PointInfo();
		mLinePaintCoords.setColor(Color.BLUE);
		mLinePaintCoords.setStrokeWidth(5);
		mLinePaintCoords.setStyle(Style.STROKE);
		mRectPaintCoords.setColor(Color.CYAN);
		mRectPaintCoords.setAlpha(50);
	}

	@Override
	public void selectObject(Object obj, PointInfo touchPoint) {
		// We aren't dragging any objects in this particular app, but this is called when the point goes up (obj == null) or down (obj != null),
		// save the touch point info
		touchPointChanged(touchPoint);
	}

	@Override
	public boolean setPositionAndScale(Object obj, PositionAndScale newImgPosAndScale, PointInfo touchPoint) {
		// Called during a drag or stretch operation, update the touch point info
		touchPointChanged(touchPoint);
		return true;
	}

	@Override
	public void getPositionAndScale(Object obj, PositionAndScale objPosAndScaleOut) {
		// We aren't dragging any objects, so this doesn't do anything in this app
	}

	@Override
	public Object getDraggableObjectAtPoint(PointInfo pt) {
		// IMPORTANT: to start a multitouch drag operation, this routine must return non-null
		return this;
	}

	@Override
	public boolean onTouchEvent(MotionEvent event) {
		// Pass the event on to the controller
		return multiTouchController.onTouchEvent(event);
	}

	@Override
	/**
	 * Draws a box defined by the user's two fingers. The box ends when there are less than two fingers on the screen.
	 */
	protected void onDraw(Canvas canvas) {
		super.onDraw(canvas);
		if (mCurrTouchPoint.isDown()) {
			int numPoints = mCurrTouchPoint.getNumTouchPoints();
			float x = mCurrTouchPoint.getX(), y = mCurrTouchPoint.getY();

			if (numPoints == 2) {
				//dx2 and dy2 can be thought of as the "deviation" from the selection box's center in the x- and y- direction
				float dx2 = mCurrTouchPoint.getMultiTouchWidth() / 2;
				float dy2 = mCurrTouchPoint.getMultiTouchHeight() / 2;

				//Right side of box
				canvas.drawLine(x + dx2, y - dy2, x + dx2, y + dy2, mLinePaintCoords);
				//Left side of box
				canvas.drawLine(x - dx2, y - dy2, x - dx2, y + dy2, mLinePaintCoords);
				//Bottom side of box
				canvas.drawLine(x - dx2, y + dy2, x + dx2, y + dy2, mLinePaintCoords);
				//Top side of box
				canvas.drawLine(x - dx2, y - dy2, x + dx2, y - dy2, mLinePaintCoords);
				//Draws in transparent shading inside box
				canvas.drawRect(x - dx2, y - dy2, x + dx2, y + dy2, mRectPaintCoords);
			}
		}
	}

	/**
	 * Called when the touch point info changes, causes a redraw.
	 * 
	 * @param touchPoint
	 */
	private void touchPointChanged(PointInfo touchPoint) {
		// Take a snapshot of touch point info, the touch point is volatile
		mCurrTouchPoint.set(touchPoint);
		invalidate();
	}

}
