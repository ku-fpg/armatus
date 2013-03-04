package com.kufpg.androidhermit.server;

import android.app.ProgressDialog;
import android.os.AsyncTask;

import org.json.JSONException;
import org.json.JSONObject;

import com.kufpg.androidhermit.console.ConsoleActivity;

public class HermitServer extends AsyncTask<JSONObject, Void, JSONObject> {

	private ProgressDialog mDialog;
	private ConsoleActivity mConsole;
	private JSONObject mJsonObject;

	public HermitServer(ConsoleActivity console, JSONObject request) {
		mConsole = console;
		mDialog = new ProgressDialog(console);
		mDialog.setIndeterminate(true);
		mDialog.setCancelable(false);
		mDialog.setCanceledOnTouchOutside(false);
		mDialog.setMessage("Accessing HERMIT server");
		mDialog.show();
	}

	//WARNING: Do not use mConsole in doInBackground!
	@Override
	protected JSONObject doInBackground(JSONObject... params) {
		// TODO Interface with actual Hermit server 
		// For now, just delay a few seconds to simulate network activity
		for (long i = 0; i < 99999999; i++) {
			double j = i / 2.0;
			j = j - 1;
		}

		String jstr = "{text:\"server response\"}";
		JSONObject res = null;
		try {
			res = new JSONObject(jstr);
		} catch (Exception e){
			e.printStackTrace();
		}
		return res;
	}

	@Override
	protected void onPostExecute(JSONObject result) {
		if (mConsole != null) {
			mDialog.dismiss();
			try {
				mConsole.appendConsoleEntry(result.getString("text"));
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
	}

	public void setConsole(ConsoleActivity console) {
		mConsole = console;
	}
	
	public void setJsonObject(JSONObject json) {
		mJsonObject = json;
	}

}
