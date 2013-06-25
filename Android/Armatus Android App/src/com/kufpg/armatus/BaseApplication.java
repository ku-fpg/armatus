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
	private Map<String, List<AsyncActivityTask<?,?,?>>> mActivityTaskMap;

	public BaseApplication() {
		mActivityTaskMap = new HashMap<String, List<AsyncActivityTask<?,?,?>>>();
	}

	public void removeActivityTask(AsyncActivityTask<?,?,?> task) {
		for (Entry<String, List<AsyncActivityTask<?,?,?>>> entry : mActivityTaskMap.entrySet()) {
			List<AsyncActivityTask<?,?,?>> tasks = entry.getValue();
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

	public void addActivityTask(Activity activity, AsyncActivityTask<?,?,?> task) {
		String key = activity.getClass().getCanonicalName();
		List<AsyncActivityTask<?,?,?>> tasks = mActivityTaskMap.get(key);
		if (tasks == null) {
			tasks = new ArrayList<AsyncActivityTask<?,?,?>>();
			mActivityTaskMap.put(key, tasks);
		}

		tasks.add(task);
	}

	public void detach(Activity activity) {
		List<AsyncActivityTask<?,?,?>> tasks = mActivityTaskMap.get(activity.getClass().getCanonicalName());
		if (tasks != null) {
			for (AsyncActivityTask<?,?,?> task : tasks) {
				task.setActivity(null);
			}
		}
	}

	public void attach(Activity activity) {
		List<AsyncActivityTask<?,?,?>> tasks = mActivityTaskMap.get(activity.getClass().getCanonicalName());
		if (tasks != null) {
			for (AsyncActivityTask<?,?,?> task : tasks) {
				task.setActivity(activity);
			}
		}
	}
}
