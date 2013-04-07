package com.kufpg.androidhermit.dialog;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import com.animoto.android.views.DraggableGridView;
import com.animoto.android.views.OnRearrangeListener;
import com.kufpg.androidhermit.R;
import com.kufpg.androidhermit.console.ConsoleActivity;

import android.app.DialogFragment;
import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.ImageView;

public class ConsoleEntryRearrangeDialog extends DialogFragment {

	private DraggableGridView mGridView;
	private Button mCoolStuffButton;
	private int mEntryNum;
	private String mEntryContents;
	private List<String> mSentence = new ArrayList<String>();
	private Random mRandom = new Random();
	
	public static ConsoleEntryRearrangeDialog newInstance(int entryNum, String entryContents) {
		ConsoleEntryRearrangeDialog cerd = new ConsoleEntryRearrangeDialog();
		
		Bundle args = new Bundle();
		args.putInt("entryNum", entryNum);
		args.putString("entryContents", entryContents);
		cerd.setArguments(args);
		
		return cerd;
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
		View v = inflater.inflate(R.layout.console_entry_rearrange_dialog, container, false);
		setCancelable(false);

		getDialog().setTitle("Entry number " + String.valueOf(mEntryNum));
		mGridView = (DraggableGridView) v.findViewById(R.id.console_entry_rearrange_dialog_contents);
		for (String word : mEntryContents.split(ConsoleActivity.WHITESPACE)) {
			ImageView wordView = new ImageView(getActivity());
			wordView.setImageBitmap(getThumb(word));
			mGridView.addView(wordView);
			mSentence.add(word);
		}
		mGridView.setOnRearrangeListener(new OnRearrangeListener() {
			public void onRearrange(int oldIndex, int newIndex) {
				String word = mSentence.remove(oldIndex);
				if (oldIndex < newIndex)
					mSentence.add(newIndex, word);
				else
					mSentence.add(newIndex, word);
			}
		});
		
		mCoolStuffButton = (Button) v.findViewById(R.id.console_entry_rearrange_done);
		mCoolStuffButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				String newSentence = "";
				for (String word : mSentence) {
					newSentence += word;
				}
				((ConsoleActivity) getActivity()).showToast(newSentence);
			}
		});
		
		return v;
	}
	
	private Bitmap getThumb(String s) {
		Bitmap bmp = Bitmap.createBitmap(150, 150, Bitmap.Config.RGB_565);
		Canvas canvas = new Canvas(bmp);
		Paint paint = new Paint();

		paint.setColor(Color.rgb(mRandom.nextInt(128), mRandom.nextInt(128), mRandom.nextInt(128)));
		paint.setTextSize(24);
		paint.setFlags(Paint.ANTI_ALIAS_FLAG);
		canvas.drawRect(new Rect(0, 0, 150, 150), paint);
		paint.setColor(Color.WHITE);
		paint.setTextAlign(Paint.Align.CENTER);
		canvas.drawText(s, 75, 75, paint);

		return bmp;
	}

}
