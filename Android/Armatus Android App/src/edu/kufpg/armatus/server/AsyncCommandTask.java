package edu.kufpg.armatus.server;

import android.app.Activity;
import android.os.AsyncTask;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.CommandDispatcher;

/**
 * A task that is launched by a {@link CommandDispatcher.Command Command} that runs asynchronously.
 * This class is needed in order to properly notify the {@link CommandDispatcher} when there is
 * no longer a pending {@code Command} to execute.
 * @param <A> The {@code Activity} class this task is associated with.
 * @param <Params> The {@code Object} that this task takes as input when calling {@link
 * AsyncTask#execute(Params...) execute(Params...)}.
 * @param <Progress> The {@code Object} that this task uses to determine execution progress.
 * @param <Result> The {@code Object} that this task returns when execution is completed.
 */
public abstract class AsyncCommandTask<A extends Activity, Params, Progress, Result> extends AsyncActivityTask<A, Params, Progress, Result> {

	public AsyncCommandTask(A activity) {
		super(activity);
	}

	@Override
	protected void onPostExecute(Result result) {
		super.onPostExecute(result);

		if (CommandDispatcher.isCommandPending()) {
			CommandDispatcher.notifyDelayedCommandFinished();
		}
	}

}
