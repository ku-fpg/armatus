package edu.kufpg.armatus;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.TextView;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.tutorial.TutorialConsoleActivity;
import edu.kufpg.armatus.dialog.TerminalNotInstalledDialog;
import edu.kufpg.armatus.treelistview.TreeListViewDemo;
import edu.kufpg.armatus.util.StickyButton;

/**
 * The {@link Activity} that is opened when the app is first started. This is merely a
 * crossroads for all of the various demos of features that may one day make it into the
 * final Armatus app.
 */
public class MainActivity extends BaseActivity implements OnClickListener {
    private TextView mButtonsView;
    private StickyButton mStickyButton;
    private int mNumTextChanges = 0;

    @Override protected void onCreate(@Nullable final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main_activity);

        mButtonsView = (TextView) findViewById(R.id.code_text_view);
        final Button consoleButton = (Button) findViewById(R.id.console_button);
        final Button tutorialButton = (Button) findViewById(R.id.tutorial_button);
        mStickyButton = (StickyButton) findViewById(R.id.lock_button);
        final Button unstickButton = (Button) findViewById(R.id.unlock_button);
        final Button treeButton = (Button) findViewById(R.id.tree_button);
        final Button terminalButton = (Button) findViewById(R.id.terminal_activity_button);

        consoleButton.setOnClickListener(this);
        tutorialButton.setOnClickListener(this);
        mStickyButton.setOnClickListener(this);
        unstickButton.setOnClickListener(this);
        treeButton.setOnClickListener(this);
        terminalButton.setOnClickListener(this);

        if (savedInstanceState != null) {
            mNumTextChanges = savedInstanceState.getInt("numTextChanges");
        }
        setCodeText(mNumTextChanges);
    }

    @Override protected void onSaveInstanceState(@NonNull final Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putInt("numTextChanges", mNumTextChanges);
    }

    @Override public void onClick(@NonNull final View v) {
        switch (v.getId()) {
            case R.id.console_button:
                startActivity(new Intent(this, ConsoleActivity.class));
                break;
            case R.id.tutorial_button:
                startActivity(new Intent(this, TutorialConsoleActivity.class));
                break;
            case R.id.lock_button:
                mNumTextChanges++;
                setCodeText(mNumTextChanges);
                break;
            case R.id.unlock_button:
                mStickyButton.unstick();
                setCodeText(mNumTextChanges);
                break;
            case R.id.tree_button:
                startActivity(new Intent(this, TreeListViewDemo.class));
                break;
            case R.id.terminal_activity_button:
                String packageName = "jackpal.androidterm";
                boolean installed = appInstalledOrNot(this, packageName);
                if (installed) {
                    Intent i = new Intent("jackpal.androidterm.RUN_SCRIPT");
                    i.addCategory(Intent.CATEGORY_DEFAULT);
                    i.putExtra("jackpal.androidterm.iInitialCommand", "echo 'Hello, Armatus!'");
                    startActivity(i);
                } else {
                    TerminalNotInstalledDialog tnid = new TerminalNotInstalledDialog();
                    tnid.show(getFragmentManager(), "tnid");
                }
                break;
        }
    }

    private void setCodeText(final int numTextChanges) {
        mButtonsView.setText("Button pushed " + numTextChanges + " times. (Status: "
                + (mStickyButton.isStuck() ? "locked" : "unlocked") + ".)");
    }
}
