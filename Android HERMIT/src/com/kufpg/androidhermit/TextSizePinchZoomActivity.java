package com.kufpg.androidhermit;

import com.kufpg.androidhermit.util.ConsoleTextView;

import android.os.Bundle;
import android.app.Activity;
import android.util.Log;
import android.util.TypedValue;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.view.ScaleGestureDetector.SimpleOnScaleGestureListener;
import android.widget.TextView;

public class TextSizePinchZoomActivity extends Activity {

    TextView scaleGesture;
    ScaleGestureDetector scaleGestureDetector;
    private final static int DEFAULT_FONT_SIZE = ConsoleActivity.DEFAULT_FONT_SIZE;
	private final static int MAX_FONT_SIZE = ConsoleActivity.MAX_FONT_SIZE;
	private final static int MIN_FONT_SIZE = ConsoleActivity.MIN_FONT_SIZE;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.textsize_pinchzoom);
        scaleGesture = (TextView)findViewById(R.id.resize_me);
        scaleGesture.setText("this is some text");
        scaleGesture.setTextSize(DEFAULT_FONT_SIZE);
        scaleGestureDetector = new ScaleGestureDetector(this, new simpleOnScaleGestureListener());
    }


    @Override
    public boolean onTouchEvent(MotionEvent event) {
        // TODO Auto-generated method stub
        scaleGestureDetector.onTouchEvent(event);
        return true;
    }

    public class simpleOnScaleGestureListener extends
            SimpleOnScaleGestureListener {

        @Override
        public boolean onScale(ScaleGestureDetector detector) {
            // TODO Auto-generated method stub
            float size = scaleGesture.getTextSize();
            Log.d("TextSizeStart", String.valueOf(size));

            float factor = detector.getScaleFactor();
            Log.d("Factor", String.valueOf(factor));


            float product = size*factor;
            product = Math.max(MIN_FONT_SIZE, Math.min(product, MAX_FONT_SIZE));
            Log.d("TextSize", String.valueOf(product));
            scaleGesture.setTextSize(TypedValue.COMPLEX_UNIT_PX, product);

            size = scaleGesture.getTextSize();
            Log.d("TextSizeEnd", String.valueOf(size));
            return true;
        }
    }
}