package edu.kufpg.armatus.activity;

import android.content.Context;
import android.content.Intent;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.ConsoleEntry;
import edu.kufpg.armatus.console.HermitClient;

public class ConsoleEntryIntent extends Intent {

	public static final String ENTRY_TAG = "entry";
	public static final String CLIENT_TAG = "client";
	
	public ConsoleEntryIntent(ConsoleEntry entry,
			ConsoleActivity console, Class<? extends ConsoleEntryActivity> cls) {
		super(console, cls);
		putExtra(ENTRY_TAG, entry);
		putExtra(CLIENT_TAG, console.getHermitClient());
	}
	
	public ConsoleEntryIntent(ConsoleEntry entry, HermitClient client,
			Context packageContext, Class<? extends ConsoleEntryActivity> cls) {
		super(packageContext, cls);
		putExtra(ENTRY_TAG, entry);
		putExtra(CLIENT_TAG, client);
	}
	
}
