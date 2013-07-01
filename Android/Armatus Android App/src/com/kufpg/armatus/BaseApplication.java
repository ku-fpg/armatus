package com.kufpg.armatus;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;

import android.app.Activity;
import android.app.Application;

/**
 * This class manages the connection between an Activity and the AsyncActivityTasks
 * that it spawns.
 * @param <A> The Activity class being managed.
 */
public class BaseApplication<A extends Activity> extends Application {
	/**
	 * An Activity can spawn any number of AsyncTasks simultaneously, so use a
	 * Multimap to connect an Activity's name and its AsyncActivityTasks.
	 */
	private ListMultimap<String, AsyncActivityTask<A,?,?,?>> mActivityTaskMap = ArrayListMultimap.create();

	/**
	 * Removes unused AsyncActivityTasks after they have completed execution.
	 * @param activity The Activity that spawned the AsyncActivityTask.
	 * @param task The AsyncActivityTask to remove.
	 */
	public void removeTask(A activity, AsyncActivityTask<A,?,?,?> task) {
		mActivityTaskMap.remove(activity.getClass().getCanonicalName(), task);
	}

	/**
	 * Establishes a connection between an Activity and a AsyncActivityTask that will
	 * persist through device rotation or standby.
	 * @param activity The Activity that spawned the AsyncActivityTask.
	 * @param task The AsyncActivityTask to connect.
	 */
	public void addTask(A activity, AsyncActivityTask<A,?,?,?> task) {
		mActivityTaskMap.put(activity.getClass().getCanonicalName(), task);
	}

	/**
	 * While the Activity rotates or is in standby, attempting to call an Activity
	 * method from one of its AsyncActivityTasks can produce unexpected results.
	 * Use this method to set all of an activity's references in its tasks to null
	 * so that the tasks can work around rotation or standby.
	 * @param activity The Activity whose references should be set to null.
	 */
	public void detach(A activity) {
		for (AsyncActivityTask<A,?,?,?> task : mActivityTaskMap.get(activity.getClass().getCanonicalName())) {
			task.setActivity(null);
		}
	}

	/**
	 * Reestablishes the connection between an Activity and its AsyncActivityTasks
	 * after the Activity comes back into focus.
	 * @param activity The Activity whose references should be reestablished.
	 */
	public void attach(A activity) {
		for (AsyncActivityTask<A,?,?,?> task : mActivityTaskMap.get(activity.getClass().getCanonicalName())) {
			task.setActivity(activity);
		}
	}
}
