package com.kufpg.armatus.server;

import java.io.Serializable;

import android.os.AsyncTask;

import org.json.JSONObject;

import com.kufpg.armatus.console.ConsoleActivity;

public class HermitServer extends AsyncTask<JSONObject, Void, String> implements Serializable {

	private static final long serialVersionUID = -1927958718472477046L;
	private ConsoleActivity mConsole;
	private boolean mDone = false;

	public HermitServer(ConsoleActivity console, JSONObject request) {
		mConsole = console;
		mConsole.appendProgressSpinner();
		mConsole.disableInput();
		mConsole.setServer(this);
	}

	//WARNING: Do not use mConsole in doInBackground!
	@Override
	protected String doInBackground(JSONObject... params) {
		// TODO Interface with actual Hermit server 
		// For now, just delay a few seconds to simulate network activity
		for (long i = 0; i < 49999999 && !isCancelled(); i++) {}

		String response = null;
		if (!isCancelled()) {
			String jstr = "{text:\"server response\"}";
			try {
				JSONObject resJson = new JSONObject(jstr);
				response = resJson.getString("text");
			} catch (Exception e) {
				e.printStackTrace();
			}
		}

		mDone = true;
		return response;
	}

	@Override
	protected void onPostExecute(final String response) {
		if (response != null) {
			mConsole.appendConsoleEntry(response);
		} else {
			mConsole.appendConsoleEntry("Error: server request cancelled.");
		}
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

}
