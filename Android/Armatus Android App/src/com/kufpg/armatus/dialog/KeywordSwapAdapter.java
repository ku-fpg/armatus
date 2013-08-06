package com.kufpg.armatus.dialog;

import java.util.Collections;
import java.util.List;

import com.kufpg.armatus.R;

import android.content.ClipData;
import android.content.Context;
import android.graphics.Canvas;
import android.graphics.Paint;
import android.graphics.Point;
import android.view.DragEvent;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.View;
import android.view.View.OnDragListener;
import android.view.View.OnTouchListener;
import android.view.ViewGroup;
import android.view.View.DragShadowBuilder;
import android.widget.ArrayAdapter;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

/**
 * KeywordSwapAdpater Class and extends {@link android.widget.ArrayAdapter ArrayAdapter}. 
 */
public class KeywordSwapAdapter extends ArrayAdapter<String> {
	private static int ITEM_HIGHLIGHT = -1;
	private LayoutInflater mInflater;
	private Context mContext;
	private List<String> mKeywords;

	/**
	 * KeywordSwapAdapter Function/Constructor. Creats a inflater from the context that is passed in.
	 * @param {@link android.content.Context context}
	 * @param {@link java.util.List keywords}
	 */
	public KeywordSwapAdapter(Context context, List<String> keywords) {
		super(context, R.layout.keyword_swap_item, keywords);
		mInflater = LayoutInflater.from(context);
		mContext = context;
		mKeywords = keywords;
	}

	@Override
	public View getView(final int position, View convertView, ViewGroup parent) {
		final KeywordViewHolder holder;
		if (convertView == null) {
			convertView = mInflater.inflate(R.layout.keyword_swap_item, parent, false);
			holder = new KeywordViewHolder();
			holder.layout = (LinearLayout) convertView.findViewById(R.id.keyword_swap_item_layout);
			holder.icon = (ImageView) convertView.findViewById(R.id.keyword_swap_icon);
			holder.text = (TextView) convertView.findViewById(R.id.keyword_swap_text);
			convertView.setTag(holder);
		} else {
			holder = (KeywordViewHolder) convertView.getTag();
		}

		if (ITEM_HIGHLIGHT == -1) {
			ITEM_HIGHLIGHT = mContext.getResources().getColor(android.R.color.holo_orange_dark);
		}

		holder.layout.setOnDragListener(new OnDragListener() {
			@Override
			public boolean onDrag(View v, DragEvent event) {
				switch (event.getAction()) {
				case DragEvent.ACTION_DRAG_ENTERED: {
					int otherPos = (Integer) event.getLocalState();
					if (canBeSwapped(position, otherPos)) {
						holder.layout.setBackgroundColor(ITEM_HIGHLIGHT);
					}
					break;
				}
				case DragEvent.ACTION_DRAG_ENDED: {
					holder.layout.setBackground(null);
					break;
				}
				case DragEvent.ACTION_DRAG_EXITED: {
					holder.layout.setBackground(null);
					break;
				}
				case DragEvent.ACTION_DROP: {
					holder.layout.setBackground(null);
					int otherPos = (Integer) event.getLocalState();
					if (canBeSwapped(position, otherPos)) {
						Collections.swap(mKeywords, position, otherPos);
						notifyDataSetChanged();
					}
					break;
				}
				}
				return true;
			}
		});
		if (canBeDragged(position)) {
			holder.icon.setVisibility(View.VISIBLE);
			holder.icon.setOnTouchListener(new OnTouchListener() {
				@Override
				public boolean onTouch(View v, MotionEvent ev) {
					if (ev.getAction() == MotionEvent.ACTION_DOWN &&
							canBeDragged(position)) {
						ClipData dragData = ClipData.newPlainText("", "");
						DragShadowBuilder builder = new DragShadowBuilder(holder.layout) {
							@Override
							public void onDrawShadow(Canvas canvas) {
								super.onDrawShadow(canvas);
								Paint rectPaint = new Paint();
								rectPaint.setColor(mContext.getResources().getColor(android.R.color.holo_orange_dark));
								rectPaint.setAlpha(55);
								rectPaint.setStrokeWidth(5);

								canvas.drawRect(0, 0, holder.layout.getWidth(), holder.layout.getHeight(), rectPaint);
							}

							@Override
							public void onProvideShadowMetrics(Point shadowSize, Point shadowTouchPoint) {
								super.onProvideShadowMetrics(shadowSize, shadowTouchPoint);
								shadowTouchPoint.x = 0;
								shadowTouchPoint.y = (int) (holder.layout.getHeight() / 2.0);
							}
						};
						holder.layout.startDrag(dragData, builder, position, 0);
						return true;
					}
					return false;
				}
			});
		} else {
			holder.icon.setVisibility(View.INVISIBLE);
			holder.icon.setOnTouchListener(null);
		}

		holder.text.setText(getItem(position));

		return convertView;
	}

	/**
	 * Boolean function of canBeGragged. Reads from the index and runs a if statements that returns true or false
	 * @param index
	 * @return
	 */
	public boolean canBeDragged(int index) {
		if (index == 0 || index == getCount() - 1) {
			return true;
		}
		return false;
	}

	/**
	 * Boolean function of canBeSwapped. Reads from the pos1 and pos2, then runs a if statements that returns true or false
	 * @param pos1
	 * @param pos2
	 * @return
	 */
	private boolean canBeSwapped(int pos1, int pos2) {
		if ((pos1 == 0 && pos2 == getCount() - 1) ||
				(pos2 == 0 && pos1 == getCount() - 1)) {
			return true;
		} else {
			return false;
		}
	}
	
	/**
	 * Static class KeywordViewHolder. Has three static variables in it with static values.
	 */
	private static class KeywordViewHolder {
		public LinearLayout layout;
		public ImageView icon;
		public TextView text;
	}

}
