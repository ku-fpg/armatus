package edu.kufpg.armatus.dialog;

import java.util.ArrayList;
import java.util.Collection;

import edu.kufpg.armatus.R;
import edu.kufpg.armatus.util.StringUtils;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.TextView;

/**
 * WordCompletionDialog Class, this extends the {@link android.app.DialogFragment DialogFragment} class. 
 * This class allows for the application to complete words in the input.
 */
public class InputCompletionDialog extends ConsiderateDialog {

	private int mReplaceIndex;
	private ArrayList<String> mWords;

	/**
	 * WordCompletionDialog Instance Function. This function creates a new instance for the word completion that returns an argument.
	 * @param {@link java.lang.String words}
	 * @return
	 */
	public static InputCompletionDialog newInstance(int replaceIndex, Collection<String> words) {
		InputCompletionDialog wcd = new InputCompletionDialog();

		Bundle args = new Bundle();
		args.putInt("index", replaceIndex);
		args.putStringArrayList("words", new ArrayList<String>(words));
		wcd.setArguments(args);

		return wcd;
	}

	@Override
	public void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		mReplaceIndex = getArguments().getInt("index");
		mWords = getArguments().getStringArrayList("words");
	}

	@Override
	public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
		super.onCreateView(inflater, container, savedInstanceState);
		View v = inflater.inflate(R.layout.word_completion_dialog, container, false);
		setCancelable(true);
		getDialog().setTitle("Word completion");

		ListView listView = (ListView) v.findViewById(R.id.word_suggestions_list);
		listView.setAdapter(new ArrayAdapter<String>(getActivity(), android.R.layout.simple_list_item_1, mWords));
		listView.setOnItemClickListener(new OnItemClickListener() {
			@Override
			public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
				getConsole().setInputText(mReplaceIndex, getConsole().getInputLength(),
						((TextView) view).getText().toString() + StringUtils.NBSP);
				dismiss();
			}
		});

		return v;
	}

}