package com.kufpg.armatus;

import android.app.Activity;
import android.os.AsyncTask;

public abstract class AsyncActivityTask<A extends Activity, Params, Progress, Result> extends AsyncTask<Params, Progress, Result> {
	private BaseApplication<A> mApp;
	private A mActivity, mInterruptedActivity;

	@SuppressWarnings("unchecked")
	public AsyncActivityTask(A activity) {
		mActivity = activity;
		mInterruptedActivity = activity;
		mApp = (BaseApplication<A>) mActivity.getApplication();
	}
	
	public A getActivity() {
		if (mActivity != null) {
			return mActivity;
		} else {
			return mInterruptedActivity;
		}
	}
	

	public void setActivity(A activity) {
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
		mApp.addActivityTask(getActivity(), this);
	}

	@Override
	protected void onPostExecute(Result result) {
		mApp.removeActivityTask(getActivity(), this);
	}

	@Override
	protected void onCancelled() {
		mApp.removeActivityTask(getActivity(), this);
	}
}
