package edu.kufpg.armatus.networking;

import java.io.IOException;

import org.apache.http.HttpException;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.util.EntityUtils;

import edu.kufpg.armatus.console.ConsoleActivity;

public abstract class HermitHttpGetRequest<Result> extends HermitWebServerRequest<Result> {

	/** The URL used for performing HTTP GET requests. */
	private static final String SERVER_URL_GET = "https://raw.github.com/flori/json/master/data/example.json";

	public HermitHttpGetRequest(ConsoleActivity console) {
		super(console);
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
			httpClient = new DefaultHttpClient(httpParams);
			final HttpGet get = new HttpGet(SERVER_URL_GET);
			if (!isCancelled()) {
				httpResponse = httpClient.execute(get);
			}

			if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
				final String entity = EntityUtils.toString(httpResponse.getEntity()).trim();
				if (entity != null) {
					responseStr = entity;
				}
			} else {
				throw new HttpException(httpResponse.getStatusLine().getReasonPhrase());
			}
		} catch (HttpException e) {
			e.printStackTrace();
			responseStr = e.getMessage();
		} catch (ClientProtocolException e) {
			e.printStackTrace();
			responseStr = "ERROR: client protocol problem.";
		} catch (IOException e) {
			e.printStackTrace();
			responseStr = "ERROR: I/O problem.";
		} finally {
			httpClient.getConnectionManager().shutdown();
		}

		return onResponse(responseStr);
	}

	@Override
	protected void onPostExecute(Result formattedResponse) {
		super.onPostExecute(formattedResponse);
	}

	protected abstract Result onResponse(String response);

}
