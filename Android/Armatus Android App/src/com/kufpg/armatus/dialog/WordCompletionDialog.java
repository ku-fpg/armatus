package com.kufpg.armatus.dialog;

import java.io.Serializable;
import java.util.List;

import com.kufpg.armatus.R;
import com.kufpg.armatus.console.ConsoleActivity;

import android.app.DialogFragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

public class WordCompletionDialog extends DialogFragment {

	private List<String> mWords;

	public static WordCompletionDialog newInstance(List<String> words) {
		WordCompletionDialog wcd = new WordCompletionDialog();

		Bundle args = new Bundle();
		args.putSerializable("words", (Serializable) words);
		wcd.setArguments(args);

		return wcd;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mWords = (List<String>) getArguments().getSerializable("words");
	}

	@SuppressWarnings("unchecked")
	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.word_completion_dialog, container, false);
		setCancelable(true);

		getDialog().setTitle("Word completion");
		if (savedInstanceState != null) {
			mWords = (List<String>) savedInstanceState.getSerializable("words");
		}

		ListView listView = (ListView) v.findViewById(R.id.word_suggestions_list);
		listView.setAdapter(new ArrayAdapter<String>(getActivity(), android.R.layout.simple_list_item_1, mWords));
		listView.setOnItemClickListener(new OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				((ConsoleActivity) getActivity()).setInputText(((TextView) view).getText().toString() + " ");
				((ConsoleActivity) getActivity()).setSoftKeyboardVisible(true);
				dismiss();
			}
		});

		return v;
	}

	@Override
	public void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putSerializable("words", (Serializable) mWords);
	}

}