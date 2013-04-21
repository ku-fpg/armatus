package com.kufpg.androidhermit.dialog;

import ca.laplanete.mobile.pageddragdropgrid.PagedDragDropGrid;

import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.drag.KeywordSwapGridAdapter;

import android.app.DialogFragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;

public class KeywordSwapDialog extends DialogFragment {
	
	private int mEntryNum;
	private String mEntryContents;
	private PagedDragDropGrid mGridView;
	private Button mResetButton, mCoolStuffButton;
	
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
		mGridView = (PagedDragDropGrid) v.findViewById(R.id.keyword_swap_grid);
		mGridView.setAdapter(new KeywordSwapGridAdapter(getActivity(), mGridView, mEntryContents));
		mResetButton = (Button) v.findViewById(R.id.keyword_swap_reset_button);
		mResetButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mGridView.setAdapter(new KeywordSwapGridAdapter(getActivity(), mGridView, mEntryContents));
                mGridView.notifyDataSetChanged();
			}
		});
		mCoolStuffButton = (Button) v.findViewById(R.id.keyword_swap_coolstuff_button);
		
		return v;
	}

}
