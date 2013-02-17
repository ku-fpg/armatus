package com.kufpg.androidhermit.util;

import android.app.ProgressDialog;
import android.content.Context;

import android.os.AsyncTask;

import org.json.JSONException;
import org.json.JSONObject;

import com.kufpg.androidhermit.console.ConsoleActivity;

public class HermitServer extends AsyncTask<JSONObject, Void, JSONObject>{
	
	private final ProgressDialog mDialog;
	private static ConsoleActivity mConsole;
	
	public HermitServer(JSONObject request, ConsoleActivity console,Context context) {		
		mDialog = new ProgressDialog(context);
		mConsole = console;
		mDialog.setMessage("Accessing Hermit Server");
		mDialog.show();

	}

	@Override
	protected JSONObject doInBackground(JSONObject... params) {
		// TODO Interface with actual Hermit server 
		// For now, just delay a few seconds to simulate network activity

		for (long i = 0; i < 99999999; i++);

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
		mDialog.dismiss();		
		try {
		mConsole.addMessage(result.getString("text"));
		} catch (JSONException e) {
			e.printStackTrace();
		}	

	}

}
