package com.kufpg.armatus.dialog;

import java.util.List;

import com.kufpg.armatus.R;
import com.kufpg.armatus.console.ConsoleActivity;
import com.kufpg.armatus.console.WordCompleter;

import android.app.DialogFragment;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

public class WordCompletionDialog extends DialogFragment {

	private WordCompleter mWordCompleter;

	public static WordCompletionDialog newInstance(WordCompleter completer) {
		WordCompletionDialog wcd = new WordCompletionDialog();

		Bundle args = new Bundle();
		args.putSerializable("wordCompleter", completer);
		wcd.setArguments(args);

		return wcd;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mWordCompleter = (WordCompleter) getArguments().getSerializable("wordCompleter");
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.word_completion_dialog, container, false);
		setCancelable(true);

		getDialog().setTitle("Word completion");
		List<String> words = mWordCompleter.getWordSuggestions();

		ListView listView = (ListView) v.findViewById(R.id.word_suggestions_list);
		listView.setAdapter(new ArrayAdapter<String>(getActivity(), android.R.layout.simple_list_item_1, words));
		listView.setOnItemClickListener(new OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				((ConsoleActivity) getActivity()).setInputText(((TextView) view).getText().toString() + " ");
				getActivity().getWindow().setSoftInputMode(WindowManager.LayoutParams.SOFT_INPUT_STATE_ALWAYS_VISIBLE);
				dismiss();
			}
		});

		return v;
	}

}
