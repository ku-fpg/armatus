package edu.kufpg.armatus.networking;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpException;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.apache.http.protocol.HTTP;
import org.apache.http.util.EntityUtils;

import edu.kufpg.armatus.console.ConsoleActivity;

public abstract class HermitHttpPostRequest<Result> extends HermitWebServerRequest<Result> {
	/** The URL used for performing HTTP POST requests. */
	private static final String SERVER_URL_POST = "http://posttestserver.com/post.php?";

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

			final List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>();
			nameValuePairs.add(new BasicNameValuePair("Armatus", "is"));
			nameValuePairs.add(new BasicNameValuePair("super", "awesome"));
			
			final String requestUri = "dir=" + URLEncoder.encode("armatus", HTTP.UTF_8);

			final HttpPost httpPost = new HttpPost(SERVER_URL_POST + requestUri);
			httpPost.setEntity(new UrlEncodedFormEntity(nameValuePairs));
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
			responseStr = e.getMessage();
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
