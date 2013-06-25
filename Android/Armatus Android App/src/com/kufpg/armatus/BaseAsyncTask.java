package com.kufpg.armatus;

import android.app.Activity;
import android.os.AsyncTask;

public abstract class BaseAsyncTask<Params, Progress, Result> extends AsyncTask<Params, Progress, Result> {
	private BaseApplication mApp;
	private Activity mActivity;

	public BaseAsyncTask(Activity activity) {
		mActivity = activity;
		mApp = (BaseApplication) mActivity.getApplication();
	}

	public void setActivity(Activity activity) {
		mActivity = activity;
		if (mActivity == null) {
			onActivityDetached();
		}
		else {
			onActivityAttached();
		}
	}

	protected void onActivityAttached() {}

	protected void onActivityDetached() {}

	@Override
	protected void onPreExecute() {
		mApp.addActivityTask(mActivity, this);
	}

	@Override
	protected void onPostExecute(Result result) {
		mApp.removeActivityTask(this);
	}

	@Override
	protected void onCancelled() {
		mApp.removeActivityTask(this);
	}
}
