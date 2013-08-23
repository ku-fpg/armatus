package edu.kufpg.armatus;

import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.ListMultimap;

import android.app.Activity;
import android.app.Application;

/**
 * This class manages the connection between an application's {@link Activity Activities}
 * and the {@link AsyncActivityTask AsyncActivityTasks} that they spawn.
 * @param <A> The {@code Activity} class being managed.
 */
public class BaseApplication<A extends Activity> extends Application {
	/**
	 * An {@link Activity} can spawn any number of {@link android.os.AsyncTask AsyncTasks}
	 * simultaneously, so use a {@link ListMultimap} to connect an {@code Activity}'s name
	 * and its {@link AsyncActivityTask AsyncActivityTasks}.
	 */
	private ListMultimap<String, AsyncActivityTask<A,?,?,?>> mActivityTaskMap = ArrayListMultimap.create();

	/**
	 * Removes unused {@link AsyncActivityTask AsyncActivityTasks} after they have
	 * completed execution.
	 * @param activity The {@link Activity} that spawned the {@code AsyncActivityTask}.
	 * @param task The {@code AsyncActivityTask} to remove.
	 */
	public void removeTask(A activity, AsyncActivityTask<A,?,?,?> task) {
		mActivityTaskMap.remove(activity.getClass().getCanonicalName(), task);
	}

	/**
	 * Establishes a connection between an {@link Activity} and a {@link AsyncActivityTask}
	 * that will persist through device rotation or standby.
	 * @param activity The {@code Activity} that spawned the {@code AsyncActivityTask}.
	 * @param task The {@code AsyncActivityTask} to connect.
	 */
	public void addTask(A activity, AsyncActivityTask<A,?,?,?> task) {
		mActivityTaskMap.put(activity.getClass().getCanonicalName(), task);
	}

	/**
	 * While an {@link Activity} rotates or is in standby, attempting to call an {@code Activity}
	 * method from one of its {@link AsyncActivityTask AsyncActivityTasks} can produce
	 * unexpected results. Use this method to set all of an {@code Activity}'s references
	 * in its tasks to {@code null} so that the tasks can work around rotation or standby.
	 * @param activity The {@code Activity} whose references should be set to null.
	 */
	public void detachActivity(A activity) {
		for (AsyncActivityTask<A,?,?,?> task : mActivityTaskMap.get(activity.getClass().getCanonicalName())) {
			task.setActivity(null);
		}
	}

	/**
	 * Reestablishes the connection between an {@link Activity} and its {@link AsyncActivityTask
	 * AsyncActivityTasks} after the {@code Activity} is resumed.
	 * @param activity The {@code Activity} whose references should be reestablished.
	 */
	public void attachActivity(A activity) {
		for (AsyncActivityTask<A,?,?,?> task : mActivityTaskMap.get(activity.getClass().getCanonicalName())) {
			task.setActivity(activity);
		}
	}
}