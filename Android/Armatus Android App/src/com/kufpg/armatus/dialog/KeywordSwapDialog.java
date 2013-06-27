package com.kufpg.armatus.dialog;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.ericharlow.dragndrop.DragListener;
import com.ericharlow.dragndrop.DragNDropAdapter;
import com.ericharlow.dragndrop.DragNDropListView;
import com.ericharlow.dragndrop.DropListener;
import com.kufpg.armatus.BaseActivity;
import com.kufpg.armatus.R;
import com.kufpg.armatus.console.ConsoleActivity;

import android.app.DialogFragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.ListView;

public class KeywordSwapDialog extends DialogFragment {

	private int mEntryNum;
	private String mEntryContents;
	private List<String> mEntryWords;
	private DragNDropListView mKeywordListView;
	private DragNDropAdapter mKeywordAdapter;
	private Button mResetButton, mToastButton;

	public static KeywordSwapDialog newInstance(int entryNum, String entryContents) {
		KeywordSwapDialog ksd = new KeywordSwapDialog();

		Bundle args = new Bundle();
		args.putInt("entryNum", entryNum);
		args.putString("entryContents", entryContents);
		ksd.setArguments(args);

		return ksd;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mEntryNum = getArguments().getInt("entryNum");
		mEntryContents = getArguments().getString("entryContents");
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.keyword_swap_dialog, container, false);
		setCancelable(true);

		getDialog().setTitle("Entry number " + String.valueOf(mEntryNum));
		mEntryWords = new ArrayList<String>(Arrays.asList(mEntryContents.split(BaseActivity.WHITESPACE)));

		mKeywordListView = (DragNDropListView) v.findViewById(R.id.keyword_swap_list);
		mKeywordAdapter = new DragNDropAdapter(getActivity(), mEntryWords);
		mKeywordListView.setAdapter(mKeywordAdapter);
		mKeywordListView.setDragListener(mDragListener);
		mKeywordListView.setDropListener(mDropListener);

		mResetButton = (Button) v.findViewById(R.id.keyword_swap_reset);
		mResetButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mEntryWords = new ArrayList<String>(Arrays.asList(mEntryContents.split(BaseActivity.WHITESPACE)));
				mKeywordAdapter = new DragNDropAdapter(getActivity(), mEntryWords);
				mKeywordListView.setAdapter(mKeywordAdapter);
				mKeywordAdapter.notifyDataSetChanged();
			}
		});

		mToastButton = (Button) v.findViewById(R.id.keyword_swap_toast);
		mToastButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				String message = "";
				for (String word : mEntryWords) {
					message += word + " ";
				}
				((ConsoleActivity) getActivity()).showToast(message.trim());
			}
		});

		return v;
	}

	private DropListener mDropListener = new DropListener() {
		public void onDrop(int from, int to) {
			if (canBeSwapped(from, to)) {
				mKeywordAdapter.onDrop(from, to);
				mKeywordListView.getChildAt(to).setDrawingCacheEnabled(false);
				mKeywordListView.getChildAt(from).setDrawingCacheEnabled(false);
				mKeywordListView.invalidateViews();
			}
		}
	};

	@Override
	public void onSaveInstanceState(Bundle outState) {
		outState.putSerializable("entryWords", (Serializable) mEntryWords);
		if (mKeywordListView != null) {
			mKeywordListView.setDropListener(null);
			mKeywordListView.cancelDrag();
		}
		super.onSaveInstanceState(outState);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void onActivityCreated(Bundle savedInstanceState) {
		super.onActivityCreated(savedInstanceState);
		if (savedInstanceState != null) {
			mKeywordListView.setDropListener(mDropListener);
			mEntryWords = (List<String>) savedInstanceState.getSerializable("entryWords");
			mKeywordAdapter = new DragNDropAdapter(getActivity(), mEntryWords);
			mKeywordListView.setAdapter(mKeywordAdapter);
			mKeywordAdapter.notifyDataSetChanged();
		}
	}

	private DragListener mDragListener = new DragListener() {
		View mHighlightedView = null;
		int mDragViewColor = 0xe0103010; //Dark green
		int mHighlightColor = 0xffd17d10; //Light orange
		int mHighlightedIndex = -1;
		int mDefaultBackgroundColor;

		@Override
		public synchronized void onDrag(int draggedOverIndex, View dragView, ListView listView) {
			if (mHighlightedIndex != draggedOverIndex) {
				mHighlightedView = null;
				if (mHighlightedIndex != -1) {
					mHighlightedView = listView.getChildAt(mHighlightedIndex);
					if (mHighlightedView != null) {
						mHighlightedView.setBackgroundColor(mDefaultBackgroundColor);
					}
				}

				mHighlightedView = listView.getChildAt(draggedOverIndex);
				int draggingIndex = listView.indexOfChild(dragView);
				if (mHighlightedView != null && canBeSwapped(draggingIndex, draggedOverIndex)) {
					mHighlightedView.setBackgroundColor(mHighlightColor);
				}
				mHighlightedIndex = draggedOverIndex;
			}
		}

		@Override
		public void onStartDrag(View dragView) {
			dragView.setVisibility(View.INVISIBLE);
			mDefaultBackgroundColor = dragView.getDrawingCacheBackgroundColor();
			dragView.setBackgroundColor(mDragViewColor);
			dragView.setAlpha(0.5f);
			ImageView iv = (ImageView)dragView.findViewById(R.id.drag_indicator);
			if (iv != null) iv.setVisibility(View.INVISIBLE);
		}

		@Override
		public void onStopDrag(View dragView) {
			if (mHighlightedView != null) {
				mHighlightedView.setBackgroundColor(mDefaultBackgroundColor);
			}

			dragView.setVisibility(View.VISIBLE);
			dragView.setBackgroundColor(mDefaultBackgroundColor);
			dragView.setAlpha(1);
			ImageView iv = (ImageView)dragView.findViewById(R.id.drag_indicator);
			if (iv != null) iv.setVisibility(View.VISIBLE);
		}

	};

	private boolean canBeSwapped(int pos1, int pos2) {
		if ((pos1 == 0 && pos2 == mKeywordListView.getChildCount() - 1) ||
				(pos2 == 0 && pos1 == mKeywordListView.getChildCount() - 1)) {
			return true;
		} else {
			return false;
		}
	}

}