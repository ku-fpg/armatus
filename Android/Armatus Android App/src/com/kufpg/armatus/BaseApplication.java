package com.kufpg.armatus;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import android.app.Activity;
import android.app.Application;

public class BaseApplication extends Application {
	/**
	 * Maps between an activity class name and the list of currently running
	 * AsyncTasks that were spawned while it was active.
	 */
	private Map<String, List<BaseAsyncTask<?,?,?>>> mActivityTaskMap;

	public BaseApplication() {
		mActivityTaskMap = new HashMap<String, List<BaseAsyncTask<?,?,?>>>();
	}

	public void removeActivityTask(BaseAsyncTask<?,?,?> task) {
		for (Entry<String, List<BaseAsyncTask<?,?,?>>> entry : mActivityTaskMap.entrySet()) {
			List<BaseAsyncTask<?,?,?>> tasks = entry.getValue();
			for (int i = 0; i < tasks.size(); i++) {
				if (tasks.get(i) == task) {
					tasks.remove(i);
					break;
				}
			}

			if (tasks.size() == 0) {
				mActivityTaskMap.remove(entry.getKey());
				return;
			}
		}
	}

	public void addActivityTask(Activity activity, BaseAsyncTask<?,?,?> task) {
		String key = activity.getClass().getCanonicalName();
		List<BaseAsyncTask<?,?,?>> tasks = mActivityTaskMap.get(key);
		if (tasks == null) {
			tasks = new ArrayList<BaseAsyncTask<?,?,?>>();
			mActivityTaskMap.put(key, tasks);
		}

		tasks.add(task);
	}

	public void detach(Activity activity) {
		List<BaseAsyncTask<?,?,?>> tasks = mActivityTaskMap.get(activity.getClass().getCanonicalName());
		if (tasks != null) {
			for (BaseAsyncTask<?,?,?> task : tasks) {
				task.setActivity(null);
			}
		}
	}

	public void attach(Activity activity) {
		List<BaseAsyncTask<?,?,?>> tasks = mActivityTaskMap.get(activity.getClass().getCanonicalName());
		if (tasks != null) {
			for (BaseAsyncTask<?,?,?> task : tasks) {
				task.setActivity(activity);
			}
		}
	}
}
