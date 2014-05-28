package edu.kufpg.armatus.networking;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.SocketTimeoutException;

import org.apache.http.HttpException;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NoHttpResponseException;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.client.methods.HttpUriRequest;
import org.apache.http.conn.ConnectTimeoutException;
import org.apache.http.conn.HttpHostConnectException;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;

import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

/**
 * Task that connects to a server running HERMIT-web and simulates HERMIT commands
 * by using HTTP GET and POST requests.
 */
public abstract class HermitHttpServerRequest<Result> extends AsyncActivityTask<ConsoleActivity, String, Void, Result> {
	private HttpRequest mRequest;
	private String mErrorMessage;

	/**
	 * Constructs a new instance. The constructor is not the place to put any input
	 * {@link JSONObject}s (do that in {@link android.os.AsyncTask#execute(JSONObject...)
	 * execute(JSONObject...)} instead).
	 * @param console reference to the current console.
	 */
	public HermitHttpServerRequest(ConsoleActivity console, HttpRequest request) {
		super(console);
		mRequest = request;
	}

	@Override
	protected void onPreExecute() {
		super.onPreExecute();

		getActivity().setProgressBarVisibility(true);
		getActivity().disableInput(true);
	}

	@Override
	protected Result doInBackground(String... params) {
		HttpClient httpClient = null;
		HttpResponse httpResponse = null;
		String responseStr = null;

		try {
			final HttpParams httpParams = new BasicHttpParams();
			//Set timeout length to 10 seconds
			HttpConnectionParams.setConnectionTimeout(httpParams, 10000);
			HttpConnectionParams.setSoTimeout(httpParams, 10000);
			httpClient = new DefaultHttpClient(httpParams);
			HttpUriRequest request = null;
			if (mRequest.equals(HttpRequest.GET)) {
				request = new HttpGet(params[0]);
			} else if (mRequest.equals(HttpRequest.POST)) {
				final HttpPost httpPost = new HttpPost(params[0]);
				if (params.length > 1) {
					httpPost.setHeader("content-type", "application/json");
					if (params[1] != null && !params[1].isEmpty()) {
						try {
							httpPost.setEntity(new StringEntity(params[1]));
						} catch (UnsupportedEncodingException e) {
							e.printStackTrace();
						}
					}
				}
				request = httpPost;
			}

			if (!isCancelled()) {
				httpResponse = httpClient.execute(request);
			}

			if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
				final String entity = EntityUtils.toString(httpResponse.getEntity(), HTTP.UTF_8).trim();
				if (entity != null) {
					responseStr = entity;
				}
			} else {
				throw new HttpException("Error code " + httpResponse.getStatusLine().getStatusCode());
			}
		} catch (HttpException e) {
			return cancelResult(e, "ERROR: server problem (" + httpResponse.getStatusLine().getStatusCode() + ").");
		} catch (HttpHostConnectException e) {
			return cancelResult(e, "ERROR: server connection refused.");
		} catch (ClientProtocolException e) {
			return cancelResult(e, "ERROR: client protocol problem.");
		} catch (NoHttpResponseException e) {
			return cancelResult(e, "ERROR: the target server failed to respond.");
		} catch (ConnectTimeoutException e) {
			return cancelResult(e, "ERROR: the server connection timed out.");
		} catch (SocketTimeoutException e) {
			return cancelResult(e, "ERROR: the server connection timed out.");
		} catch (IOException e) {
			return cancelResult(e, "ERROR: I/O problem.");
		} finally {
			httpClient.getConnectionManager().shutdown();
		}

		return onResponse(responseStr);
	}

	@Override
	protected void onPostExecute(Result result) {
		super.onPostExecute(result);

		end();
	}

	@Override
	protected void onCancelled(Result error) {
		super.onCancelled(error);

		if (mErrorMessage != null && getActivity() != null) {
			getActivity().appendErrorResponse(mErrorMessage);
		}
		end();
	}

	private void end() {
		if (getActivity().getHermitClient().isRequestDelayed()) {
			getActivity().getHermitClient().notifyDelayedRequestFinished();
		}

		getActivity().enableInput();
		getActivity().setProgressBarVisibility(false);
	}

	private Result cancelResult(Exception error, String errorMsg) {
		error.printStackTrace();
		mErrorMessage = errorMsg;
		cancel(true);
		return null;
	}

	protected abstract Result onResponse(String response);
	
	protected String getErrorMessage() {
		return mErrorMessage;
	}
	
	protected void setErrorMessage(String message) {
		mErrorMessage = message;
	}

	public enum HttpRequest { GET, POST }

}
