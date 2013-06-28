package com.kufpg.armatus;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;

import android.app.Activity;
import android.app.Application;

public class BaseApplication<A extends Activity> extends Application {
	/**
	 * Maps between an activity class name and the list of currently running
	 * AsyncTasks that were spawned while it was active.
	 */
	private ListMultimap<String, AsyncActivityTask<A,?,?,?>> mActivityTaskMap = ArrayListMultimap.create();

	public void removeActivityTask(A activity, AsyncActivityTask<A,?,?,?> task) {
		mActivityTaskMap.remove(activity.getClass().getCanonicalName(), task);
	}

	public void addActivityTask(A activity, AsyncActivityTask<A,?,?,?> task) {
		mActivityTaskMap.put(activity.getClass().getCanonicalName(), task);
	}

	public void detach(A activity) {
		for (AsyncActivityTask<A,?,?,?> task : mActivityTaskMap.get(activity.getClass().getCanonicalName())) {
			task.setActivity(null);
		}
	}

	public void attach(A activity) {
		for (AsyncActivityTask<A,?,?,?> task : mActivityTaskMap.get(activity.getClass().getCanonicalName())) {
			task.setActivity(activity);
		}
	}
}
