package com.kufpg.armatus.dialog;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.google.common.collect.Lists;
import com.kufpg.armatus.R;
import com.kufpg.armatus.console.ConsoleActivity;
import com.kufpg.armatus.util.StringUtils;

import android.app.DialogFragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ListView;

public class KeywordSwapDialog extends DialogFragment {

	private int mEntryNum;
	private String mEntryContents;
	private List<String> mEntryWords;
	private ListView mKeywordListView;
	private KeywordSwapAdapter mKeywordAdapter;
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

	@SuppressWarnings("unchecked")
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.keyword_swap_dialog, container, false);
		setCancelable(true);

		getDialog().setTitle("Entry number " + String.valueOf(mEntryNum));

		mKeywordListView = (ListView) v.findViewById(R.id.keyword_swap_list);
		if (savedInstanceState == null) {
			mEntryWords = Lists.newArrayList(mEntryContents.split(StringUtils.WHITESPACE));
		} else {
			mEntryWords = (List<String>) savedInstanceState.getSerializable("entryWords");
		}
		mKeywordAdapter = new KeywordSwapAdapter(getActivity(), mEntryWords);
		mKeywordListView.setAdapter(mKeywordAdapter);

		mResetButton = (Button) v.findViewById(R.id.keyword_swap_reset);
		mResetButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mEntryWords = new ArrayList<String>(Arrays.asList(mEntryContents.split(StringUtils.WHITESPACE)));
				mKeywordAdapter = new KeywordSwapAdapter(getActivity(), mEntryWords);
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

	@Override
	public void onSaveInstanceState(Bundle outState) {
		outState.putSerializable("entryWords", (Serializable) mEntryWords);
		super.onSaveInstanceState(outState);
	}

}