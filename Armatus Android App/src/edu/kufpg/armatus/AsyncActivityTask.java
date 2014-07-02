package edu.kufpg.armatus;

import android.app.Activity;
import android.os.AsyncTask;
import android.support.annotation.NonNull;

/**
 * An {@link AsyncTask} whose execution is intimately connected with an {@link Activity}. Unlike
 * a regular {@code AsyncTask}, an {@code AsyncActivityTask} can safely interact with its associated
 * {@code Activity} in the UI thread methods ({@link #onPreExecute()}, {@link #onCancelled()}, {@link
 * android.os.AsyncTask#onProgressUpdate(Progress...) onProgressUpdate(Progress)}, and {@link #onPostExecute(Result)}),
 * even during or after {@code Activity} rotation or standby.
 *
 * @param <A> The {@code Activity} class this task is associated with.
 * @param <Params> The {@code Object} that this task takes as input when calling {@link
 * android.os.AsyncTask#execute(Params...) execute(Params...)}.
 * @param <Progress> The {@code Object} that this task uses to determine execution progress.
 * @param <Result> The {@code Object} that this task returns when execution is completed.
 */
public abstract class AsyncActivityTask<A extends Activity, Params, Progress, Result>
        extends AsyncTask<Params, Progress, Result> {

    /**
     * A simple {@link android.app.Fragment Fragment} that is retained across rotations of this task's
     * associated {@link Activity}. This is used to retrieve an up-to-date instance of the {@code Activity}.
     */
    private final TaskFragment mFragment;

    /**
     * Construct a new task, associating it with the given {@link Activity}.
     *
     * @param activity The {@code Activity} whose execution this task is tied to.
     */
    public AsyncActivityTask(@NonNull final A activity) {
        mFragment = new TaskFragment(this);
        activity.getFragmentManager()
                .beginTransaction()
                .add(mFragment, null)
                .commit();
        activity.getFragmentManager().executePendingTransactions();
    }

    /**
     * Retrieves the {@link Activity} associated with this task. Note that the {@code Activity} can be
     * detached at any time (e.g., due to a rotation), so it is not safe to store the result of {@code
     * getActivity()} and use later, e.g.:
     * <p/>
     * <pre>{@code
     * Activity a = getActivity();
     * // do some stuff
     * a.getTitle(); // Bad!
     * }
     * </pre>
     * <p/>
     * Instead, one should call {@code getActivity()} every time that the associated {@code Activity}
     * needs to be used.
     *
     * @return A (possibly fleeting) reference to the assoicated {@code Activity}.
     */
    @NonNull public A getActivity() {
        //noinspection unchecked
        return (A) mFragment.getActivity();
    }

    /**
     * Called when the task's associated {@link Activity} is attached. This usually occurs immediately
     * after the {@code Activity} finishes rotation. Note that this will not be called before the task
     * begins (see {@link #onPreExecute()}).
     */
    protected void onActivityAttached() {}

    /**
     * Called when the task's associated {@link Activity} is detached. This usually occurs immediately
     * before the {@code Activity} begins rotation. Note that this will not be called when the task
     * finishes (see {@link #onFinish}).
     */
    protected void onActivityDetached() {}

    @Override protected void onPostExecute(final Result result) {
        onFinish();
    }

    /**
     * Make sure you call {@code super.onCancelled(result)} (contrary to the advice given
     * in the documentation for {@link android.os.AsyncTask}).
     */
    @Override protected void onCancelled(final Result result) {
        onCancelled();
        onFinish();
    }

    /**
     * Called when the task has finished execution. This is called after either {@link
     * #onPostExecute(Object) onPostExecute(Result)} or {@link #onCancelled(Object)
     * onCancelled(Result}.
     */
    protected void onFinish() {
        mFragment.getFragmentManager()
                .beginTransaction()
                .remove(mFragment)
                .commitAllowingStateLoss();
        mFragment.notifyFinished();
    }

}