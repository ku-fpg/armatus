package com.kufpg.armatus;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import android.app.Activity;
import android.app.Application;

public class BaseApplication<A extends Activity> extends Application {
	/**
	 * Maps between an activity class name and the list of currently running
	 * AsyncTasks that were spawned while it was active.
	 */
	private Map<String, List<AsyncActivityTask<A,?,?,?>>> mActivityTaskMap;

	public BaseApplication() {
		mActivityTaskMap = new HashMap<String, List<AsyncActivityTask<A,?,?,?>>>();
	}

	public void removeActivityTask(AsyncActivityTask<A,?,?,?> task) {
		for (Entry<String, List<AsyncActivityTask<A,?,?,?>>> entry : mActivityTaskMap.entrySet()) {
			List<AsyncActivityTask<A,?,?,?>> tasks = entry.getValue();
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

	public void addActivityTask(A activity, AsyncActivityTask<A,?,?,?> task) {
		String key = activity.getClass().getCanonicalName();
		List<AsyncActivityTask<A,?,?,?>> tasks = mActivityTaskMap.get(key);
		if (tasks == null) {
			tasks = new ArrayList<AsyncActivityTask<A,?,?,?>>();
			mActivityTaskMap.put(key, tasks);
		}

		tasks.add(task);
	}

	public void detach(A activity) {
		List<AsyncActivityTask<A,?,?,?>> tasks = mActivityTaskMap.get(activity.getClass().getCanonicalName());
		if (tasks != null) {
			for (AsyncActivityTask<A,?,?,?> task : tasks) {
				task.setActivity(null);
			}
		}
	}

	public void attach(A activity) {
		List<AsyncActivityTask<A,?,?,?>> tasks = mActivityTaskMap.get(activity.getClass().getCanonicalName());
		if (tasks != null) {
			for (AsyncActivityTask<A,?,?,?> task : tasks) {
				task.setActivity(activity);
			}
		}
	}
}
