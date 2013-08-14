package edu.kufpg.armatus.server;

import java.io.IOException;
import java.io.InputStream;
import java.net.UnknownHostException;
import java.util.Scanner;

import org.apache.http.HttpResponse;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.conn.ConnectTimeoutException;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;

import edu.kufpg.armatus.console.ConsoleActivity;

public abstract class HermitHttpGetRequest<Result> extends HermitWebServerRequest<Result> {
	
	/** The URL used for performing HTTP GET requests. */
	private static final String SERVER_URL_GET = "https://raw.github.com/flori/json/master/data/example.json";

	public HermitHttpGetRequest(ConsoleActivity console) {
		super(console);
	}
	
	@Override
	protected Result doInBackground(String... params) {
		HttpClient client = null;
		HttpResponse httpResponse = null;
		String response;

		try {
			final HttpParams httpParams = new BasicHttpParams();
			//Set timeout length to 30 seconds
			HttpConnectionParams.setConnectionTimeout(httpParams, 30000);
			client = new DefaultHttpClient(httpParams);
			final HttpGet request = new HttpGet(SERVER_URL_GET);
			if (!isCancelled()) {
				httpResponse = client.execute(request);
			}
			final InputStream is = httpResponse.getEntity().getContent();
			Scanner scanner = new Scanner(is).useDelimiter("\\A");
			response = scanner.hasNext() ? scanner.next() : "";
			scanner.close();
		} catch (ConnectTimeoutException e) {
			e.printStackTrace();
			response = "Connection timeout error.";
		} catch (UnknownHostException e) {
			e.printStackTrace();
			response = "Unknown host error.";
		} catch (IOException e) {
			e.printStackTrace();
			response = "IO error.";
		} finally {
			client.getConnectionManager().shutdown();
		}

		return onResponse(response);
	}
	
	@Override
	protected void onPostExecute(Result formattedResponse) {
		super.onPostExecute(formattedResponse);
	}
	
	protected abstract Result onResponse(String response);

}
