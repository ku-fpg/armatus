package edu.kufpg.armatus;

import android.app.Activity;
import android.app.Fragment;
import android.os.Bundle;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import android.util.Log;

/**
 * A simple {@link Fragment} that is retained across rotations of an {@link AsyncActivityTask}'s associated
 * {@link Activity}. This allows for the task to retrieve up-to-date references to the {@code Activity}.
 */
class TaskFragment extends Fragment {
    private static final String TAG = TaskFragment.class.getSimpleName();
    private static final boolean DEBUG = false;

    /**
     * This {@link Fragment} exists alongside this task.
     */
    private final AsyncActivityTask<?, ?, ?, ?> mTask;
    /**
     * Tracks if this {@link Fragment} has been attached to its {@link Activity} for the first time. This
     * is used in {@link #onAttach(Activity)}.
     */
    private boolean mAttached = false;
    /**
     * Tracks if this {@link Fragment} has been removed from its {@link Activity} for good. This is used
     * in {@link #onDetach()}.
     */
    private boolean mFinished = false;

    /**
     * Constructs a new {@link Fragment} that exists alongside the given task.
     *
     * @param task The task that is backed by this {@code Fragment}.
     */
    public TaskFragment(@NonNull final AsyncActivityTask<?, ?, ?, ?> task) {
        mTask = task;
    }

    @Override public void onAttach(@NonNull final Activity activity) {
        super.onAttach(activity);
        if (mAttached) {
            mTask.onActivityAttached();
            if (DEBUG) {
                Log.d(TAG, "onAttach()");
            }
        } else {
            mAttached = true;
        }
    }

    @Override public void onCreate(@Nullable final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setRetainInstance(true);
    }

    @Override public void onDetach() {
        super.onDetach();
        if (!mFinished) {
            mTask.onActivityDetached();
            if (DEBUG) {
                Log.d(TAG, "onDetach()");
            }
        }
    }

    /**
     * Informs this {@link Fragment} that it has been removed from its {@link Activity} for good,
     * and {@link AsyncActivityTask#onActivityDetached()} should not be called again.
     */
    void notifyFinished() {
        mFinished = true;
    }

}