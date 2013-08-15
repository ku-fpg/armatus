package edu.kufpg.armatus.command;

import android.os.AsyncTask;
import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

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
public abstract class AsyncCommandTask<Params, Progress, Result> extends AsyncActivityTask<ConsoleActivity, Params, Progress, Result> {

	public AsyncCommandTask(ConsoleActivity console) {
		super(console);
	}

	@Override
	protected void onPostExecute(Result result) {
		super.onPostExecute(result);

		if (CommandDispatcher.isCommandPending()) {
			CommandDispatcher.notifyDelayedCommandFinished();
		}
	}

}
