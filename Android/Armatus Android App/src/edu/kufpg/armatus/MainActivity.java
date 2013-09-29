package edu.kufpg.armatus;

import org.json.JSONException;
import org.json.JSONObject;

import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.data.CommandResponse;
import edu.kufpg.armatus.data.Glyph;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import edu.kufpg.armatus.radialmenu.RadialMenuActivity;
import edu.kufpg.armatus.treelistview.TreeListViewDemo;
import edu.kufpg.armatus.util.StickyButton;
import edu.kufpg.armatus.util.TextDrawable;

import android.content.Intent;
import android.content.res.TypedArray;
import android.graphics.Color;
import android.os.Bundle;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.style.ImageSpan;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;

/**
 * The {@link Activity} that is opened when the app is first started. This is merely a
 * crossroads for all of the various demos of features that may one day make it into the
 * final Armatus app.
 */
public class MainActivity extends BaseActivity {
	private static final String TEST_DATA = "wrap = λ △ f ds →" +
			"\n  case ds of wild ▲" +
			"\n    [] → undefined ▲" +
			"\n    (:) a as → f a as";
	private static CommandResponse sResponse;

	private TextView mButtonsView, mGlyphSpanTextView;
	private StickyButton mStickyButton;
	private Button mUnstickButton, mTreeButton, mConsoleButton,
	mPinchZoomButton, mTerminalButton, mGlyphSpanButton;
	private int mNumTextChanges = 0;

	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.main_activity);

		mButtonsView = (TextView) findViewById(R.id.code_text_view);
		mStickyButton = (StickyButton) findViewById(R.id.lock_button);
		setCodeText(mNumTextChanges);
		mUnstickButton = (Button) findViewById(R.id.unlock_button);
		mTreeButton = (Button) findViewById(R.id.tree_button);
		mConsoleButton = (Button) findViewById(R.id.console_button);
		mPinchZoomButton = (Button) findViewById(R.id.radialmenu_button);
		mTerminalButton = (Button) findViewById(R.id.terminal_activity_button);
		mGlyphSpanButton = (Button) findViewById(R.id.glyph_span_button);
		mGlyphSpanTextView = (TextView) findViewById(R.id.glyph_span_text_view);

		mStickyButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mNumTextChanges++;
				setCodeText(mNumTextChanges);
			}
		});

		mUnstickButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				mStickyButton.unstick();
				setCodeText(mNumTextChanges);
			}	
		});

		mTreeButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				startActivity(new Intent(MainActivity.this, TreeListViewDemo.class));
			}
		});

		mConsoleButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				startActivity(new Intent(MainActivity.this, ConsoleActivity.class));
			}
		});

		mPinchZoomButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				startActivity(new Intent(MainActivity.this, RadialMenuActivity.class));
			}
		});

		mTerminalButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				String packageName = "jackpal.androidterm";
				boolean installed = appInstalledOrNot(MainActivity.this, packageName);  
				if (installed) {
					Intent i = new Intent("jackpal.androidterm.RUN_SCRIPT");
					i.addCategory(Intent.CATEGORY_DEFAULT);
					i.putExtra("jackpal.androidterm.iInitialCommand", "echo 'Hello, Armatus!'");
					startActivity(i);
				} else {
					TerminalNotInstalledDialog tnid = new TerminalNotInstalledDialog();
					tnid.show(getFragmentManager(), "tnid");
				}
			}
		});

		mGlyphSpanButton.setOnClickListener(new OnClickListener() {
			@Override
			public void onClick(View v) {
				int selectionStart = mGlyphSpanTextView.getSelectionStart();
				int selectionEnd = mGlyphSpanTextView.getSelectionEnd();

				if (selectionEnd - selectionStart != 0) {
					showToast(TEST_DATA.substring(selectionStart, selectionEnd));
				} else {
					showToast("No selection!");
				}
			}
		});

		//		Random rand = new Random();
		//		String copiedWord = "Test ";
		//		int words = 25;
		//		for (int i = 0; i < words; i++) {
		//			mGlyphSpanTextView.append("a");
		//		}
		//		Spannable textSpans = new SpannableString(mGlyphSpanTextView.getText());
		//		for (int i = 0; i < words; i += 1) {		
		//			int r = rand.nextInt(256);
		//			int g = rand.nextInt(256);
		//			int b = rand.nextInt(256);
		//			TextDrawable drawable = new TextDrawable(this);
		//			drawable.setText(copiedWord);
		//			drawable.setTextColor(Color.rgb(r, g, b));
		//			drawable.setTextSize(17f);
		//			drawable.setTypeface(Typeface.MONOSPACE);
		//			drawable.setBounds(0, 0, drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight());
		//			ImageSpan span = new ImageSpan(drawable, ImageSpan.ALIGN_BASELINE);
		//			textSpans.setSpan(span, i, i + 1, Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
		//		}
		//		mGlyphSpanTextView.setText(textSpans);

		try {
			sResponse = new CommandResponse(new JSONObject("{\"ast\":1,\"glyphs\":[{\"path\":[],\"text\":\"\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"}],\"text\":\"wrap\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"}],\"text\":\" \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"}],\"text\":\"=\"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"}],\"text\":\" \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"}],\"text\":\"λ\"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"}],\"text\":\" \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Var\"}],\"text\":\"△\"},{\"style\":\"TYPE\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Var\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Var\"}],\"text\":\"f\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Var\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Var\"}],\"text\":\"ds\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Var\"}],\"text\":\" \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"}],\"text\":\"→\"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"}],\"text\":\"\n  \"},{\"style\":\"KEYWORD\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"}],\"text\":\"case\"},{\"style\":\"KEYWORD\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Case_Scrutinee\"},{\"crumb\":\"Var_Id\"}],\"text\":\"ds\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Case_Scrutinee\"},{\"crumb\":\"Var_Id\"}],\"text\":\" \"},{\"style\":\"KEYWORD\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"}],\"text\":\"of\"},{\"style\":\"KEYWORD\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Case_Binder\"}],\"text\":\"wild\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Case_Binder\"}],\"text\":\" \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Case_Type\"}],\"text\":\"▲\"},{\"style\":\"TYPE\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Case_Type\"}],\"text\":\"\n    \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":0,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_Con\"}],\"text\":\"[]\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":0,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_Con\"}],\"text\":\" \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":0,\"crumb\":\"Case_Alt\"}],\"text\":\"→\"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":0,\"crumb\":\"Case_Alt\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":0,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Fun\"},{\"crumb\":\"Var_Id\"}],\"text\":\"undefined\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":0,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Fun\"},{\"crumb\":\"Var_Id\"}],\"text\":\" \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":0,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Arg\"},{\"crumb\":\"Type_Type\"}],\"text\":\"▲\"},{\"style\":\"TYPE\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":0,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Arg\"},{\"crumb\":\"Type_Type\"}],\"text\":\"\n    \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_Con\"}],\"text\":\"(\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_Con\"}],\"text\":\":\"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_Con\"}],\"text\":\")\"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_Con\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"n\":1,\"crumb\":\"Alt_Var\"}],\"text\":\"a\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"n\":1,\"crumb\":\"Alt_Var\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"n\":2,\"crumb\":\"Alt_Var\"}],\"text\":\"as\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"n\":2,\"crumb\":\"Alt_Var\"}],\"text\":\" \"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"}],\"text\":\"→\"},{\"style\":\"SYNTAX\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Fun\"},{\"crumb\":\"App_Fun\"},{\"crumb\":\"Var_Id\"}],\"text\":\"f\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Fun\"},{\"crumb\":\"App_Fun\"},{\"crumb\":\"Var_Id\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Fun\"},{\"crumb\":\"App_Arg\"},{\"crumb\":\"Var_Id\"}],\"text\":\"a\"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Fun\"},{\"crumb\":\"App_Arg\"},{\"crumb\":\"Var_Id\"}],\"text\":\" \"},{\"style\":\"VAR\",\"path\":[{\"crumb\":\"ModGuts_Prog\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Tail\"},{\"crumb\":\"ProgCons_Head\"},{\"crumb\":\"NonRec_RHS\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"crumb\":\"Lam_Body\"},{\"n\":1,\"crumb\":\"Case_Alt\"},{\"crumb\":\"Alt_RHS\"},{\"crumb\":\"App_Arg\"},{\"crumb\":\"Var_Id\"}],\"text\":\"as\"}]}"));
		} catch (JSONException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		mGlyphSpanTextView.setText(TEST_DATA);
		Spannable contentsSpans = new SpannableString(mGlyphSpanTextView.getText());
		int index = 0;
		TypedArray ta = obtainStyledAttributes(BaseActivity.getThemePrefId(), new int[] { R.attr.codeColor });
		for (Glyph glyph : sResponse.getGlyphs()) {
			String contents = glyph.getText();
			if (contents.contains("\n") || contents.contains("\t")) {
				String oneLineContents = contents.replaceAll("[\n\t]", "");
				index += contents.length() - oneLineContents.length();
				contents = oneLineContents;
			}
			if (!contents.isEmpty()) {
				TextDrawable drawable = new TextDrawable(this);
				drawable.setText(contents);
				String color = glyph.getColor();
				if (color != null) {
					drawable.setTextColor(Color.parseColor(color));
				} else {
					//drawable.setTextColor(ta.getResourceId(0, 0));
					drawable.setTextColor(Color.WHITE);
				}
				drawable.setTextSize(17f);
				drawable.setTypeface(ConsoleActivity.TYPEFACE);
				drawable.setBounds(0, 0, drawable.getIntrinsicWidth(), drawable.getIntrinsicHeight());
				ImageSpan span = new ImageSpan(drawable, ImageSpan.ALIGN_BASELINE);
				contentsSpans.setSpan(span, index, index + contents.length(), Spannable.SPAN_INCLUSIVE_EXCLUSIVE);
				index += contents.length();
			}
		}
		ta.recycle();
		mGlyphSpanTextView.setText(contentsSpans);
	}

	@Override
	protected void onSaveInstanceState(Bundle outState) {
		super.onSaveInstanceState(outState);
		outState.putInt("numTextChanges", mNumTextChanges);
	}

	@Override
	protected void onRestoreInstanceState(Bundle savedInstanceState) {
		super.onRestoreInstanceState(savedInstanceState);
		mNumTextChanges = savedInstanceState.getInt("numTextChanges");
		setCodeText(mNumTextChanges);
	}

	private void setCodeText(int numTextChanges) {
		mButtonsView.setText("Button pushed " + numTextChanges + " times. (Status: "
				+ (mStickyButton.isStuck() ? "locked" : "unlocked") + ".)");
	}

}
