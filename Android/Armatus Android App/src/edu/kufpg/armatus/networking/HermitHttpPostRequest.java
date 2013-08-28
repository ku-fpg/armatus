package edu.kufpg.armatus.networking;

import java.io.IOException;
import java.io.UnsupportedEncodingException;

import org.apache.http.HttpException;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.util.EntityUtils;

import edu.kufpg.armatus.console.ConsoleActivity;

public abstract class HermitHttpPostRequest<Result> extends HermitHttpServerRequest<Result> {

	public HermitHttpPostRequest(ConsoleActivity console) {
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

			final HttpPost httpPost = new HttpPost(params[0]);
			if (params.length > 1) {
				String jsonStr = params[1];
				httpPost.setHeader("Content-type", "application/json");
				if (jsonStr != null && !jsonStr.isEmpty()) {
					httpPost.setEntity(new StringEntity(params[1]));
				}
			}
			if (!isCancelled()) {
				httpResponse = httpClient.execute(httpPost);
			}

			if (httpResponse.getStatusLine().getStatusCode() == HttpStatus.SC_OK) {
				final String entity = EntityUtils.toString(httpResponse.getEntity()).trim();
				if (entity != null) {
					responseStr = entity;
				}
			} else {
				throw new HttpException(httpResponse.getStatusLine().getReasonPhrase());
			}
		}catch (HttpException e) {
			e.printStackTrace();
			responseStr = "ERROR: server problem (" + httpResponse.getStatusLine().getStatusCode() + ").";
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			responseStr = "ERROR: unsupported encoding.";
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

	protected abstract Result onResponse(String response);

}
