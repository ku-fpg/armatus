package edu.kufpg.armatus;

import java.lang.ref.WeakReference;

import android.app.Activity;
import android.os.AsyncTask;

/**
 * An {@link AsyncTask} whose execution is intimately connected with an {@link Activity}. Unlike
 * a regular {@code AsyncTask}, an {@code AsyncActivityTask} can safely interact with its associated
 * {@code Activity} in the UI thread methods ({@link #onPreExecute()}, {@link #onCancelled()}, {@link
 * AsyncTask#onProgressUpdate(Progress...) onProgressUpdate(Progress)}, and {@link #onPostExecute(Result)}),
 * even during or after {@code Activity} rotation or standby.
 * @param <A> The {@code Activity} class this task is associated with.
 * @param <Params> The {@code Object} that this task takes as input when calling {@link
 * AsyncTask#execute(Params...) execute(Params...)}.
 * @param <Progress> The {@code Object} that this task uses to determine execution progress.
 * @param <Result> The {@code Object} that this task returns when execution is completed.
 */
public abstract class AsyncActivityTask<A extends Activity, Params, Progress, Result> extends AsyncTask<Params, Progress, Result> {
	/**
	 * A reference to the app's {@link android.app.Application Application}, which manages the connection
	 * between this task and its associated {@link Activity}.
	 */
	private final BaseApplication mApp;
	//private final NoGuavaBaseApplication mApp;
	
	/**
	 * References this task's associated {@link Activity}. <b>DO NOT</b> access this reference
	 * directly, since there is a chance that it could be set to null due to {@code Activity}
	 * rotation or standby. Instead, use {@link #getActivity()} every time that you want to use the
	 * {@code Activity} to ensure that the correct reference is returned.
	 */
	private WeakReference<A> mActivityRef;

	public AsyncActivityTask(A activity) {
		mActivityRef = new WeakReference<A>(activity);
		mApp = (BaseApplication) activity.getApplication();
		//mApp = (NoGuavaBaseApplication) activity.getApplication();
	}

	/**
	 * Instead of accessing the connected {@link Activity} directly, use this method to
	 * ensure that the returned {@code Activity} will always be safe to use, regardless
	 * of {@code Activity} rotation or standby.
	 * @return The associated {@code Activity}.
	 */
	public A getActivity() {
		return mActivityRef.get();
	}

	/**
	 * Use this method to reestablish a connection to this task's {@link Activity} after
	 * rotation or standby.
	 * @param activity The {@code Activity} to reconnect to.
	 */
	public void attachActivity(A activity) {
		mActivityRef = new WeakReference<A>(activity);
		onActivityAttached();
	}
	
	public void detachActivity() {
		onActivityDetached();
	}

	/**
	 * Called when this task's {@link android.app.Application Application} restores the
	 * references between the task and its associated {@link Activity}. This is usually called
	 * when rotation completes or when the {@code Activity} comes back into focus after
	 * standby.
	 */
	protected void onActivityAttached() {}

	/**
	 * Called when this task's {@link android.app.Application Application} sets the references
	 * between the task and its associated {@link Activity} to {@code null}. This is usually
	 * called immediately before the {@code Activity} is rotated or put into standby.
	 */
	protected void onActivityDetached() {}

	@Override
	protected void onPreExecute() {
		mApp.addTask(getActivity(), this);
	}

	@Override
	protected void onPostExecute(Result result) {
		mApp.removeTask(getActivity(), this);
	}

	/** 
	 * Make sure you call {@code super.onCancelled(result)} (contrary to the advice given
	 * in the documentation for {@link AsyncTask}).
	 */
	@Override
	protected void onCancelled(Result result) {
		mApp.removeTask(getActivity(), this);
		onCancelled();
	}

}