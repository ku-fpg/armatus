package edu.kufpg.armatus.server;

import edu.kufpg.armatus.console.ConsoleActivity;

/**
 * Task that connects to a server running HERMIT-web and simulates HERMIT commands
 * by using HTTP GET and POST requests.
 */
public abstract class HermitWebServerRequest<Result> extends AsyncCommandTask<String, String, Result> {
	/**
	 * Constructs a new instance. The constructor is not the place to put any input
	 * {@link JSONObject}s (do that in {@link android.os.AsyncTask#execute(JSONObject...)
	 * execute(JSONObject...)} instead).
	 * @param console reference to the current console.
	 */
	public HermitWebServerRequest(ConsoleActivity console) {
		super(console);
	}

	@Override
	protected void onPreExecute() {
		super.onPreExecute();

		getActivity().setProgressBarVisibility(true);
		getActivity().disableInput();
	}

	@Override
	protected void onProgressUpdate(String... progress) {
		if (getActivity() != null) {
			for (String line : progress) {
				if (line != null && !line.isEmpty()) {
					getActivity().appendConsoleEntry(line);
				}
			}
		}
	}

	@Override
	protected void onPostExecute(Result result) {
		super.onPostExecute(result);

		end();
	}

	@Override
	protected void onCancelled() {
		super.onCancelled();

		getActivity().appendConsoleEntry("Error: server request cancelled.");
		end();
	}

	private void end() {
		getActivity().enableInput();
		getActivity().setProgressBarVisibility(false);
	}

}
