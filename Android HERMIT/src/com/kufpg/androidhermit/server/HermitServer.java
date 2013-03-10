package com.kufpg.androidhermit.server;

import java.io.Serializable;

import android.os.AsyncTask;

import org.json.JSONObject;

import com.kufpg.androidhermit.console.ConsoleActivity;

public class HermitServer extends AsyncTask<JSONObject, Void, Void> implements Serializable {

	private static final long serialVersionUID = -1927958718472477046L;
	private ConsoleActivity mConsole;
	private boolean mDone = false;
	private String mResponse;

	public HermitServer(ConsoleActivity console, JSONObject request) {
		mConsole = console;
		mConsole.appendProgressSpinner();
		mConsole.disableInput();
		mConsole.setServer(this);
	}

	//WARNING: Do not use mConsole in doInBackground!
	@Override
	protected Void doInBackground(JSONObject... params) {
		// TODO Interface with actual Hermit server 
		// For now, just delay a few seconds to simulate network activity
		for (long i = 0; i < 49999999 && !isCancelled(); i++) {}

		if (!isCancelled()) {
			String jstr = "{text:\"server response\"}";
			try {
				JSONObject res = new JSONObject(jstr);
				mResponse = res.getString("text");
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		mDone = true;
		return null;
	}

	@Override
	protected void onPostExecute(Void response) {
		appendServerResponse();
		mConsole.enableInput();
		mConsole.setServer(null);
		detach();
	}

	public void attach(ConsoleActivity console) {
		mConsole = console;
	}

	public void detach() {
		mConsole = null;
	}

	public boolean isDone() {
		return mDone;
	}

	/**
	 * WARNING: Only use this after the server request completes.
	 * @param The message to append.
	 */
	public void appendServerResponse() {
		mConsole.appendConsoleEntry(mResponse);
	}

}
