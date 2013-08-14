package edu.kufpg.armatus.server;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;

import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;

import edu.kufpg.armatus.console.ConsoleActivity;

public abstract class HermitHttpPostRequest<Result> extends HermitWebServerRequest<Result> {
	/** The URL used for performing HTTP POST requests. */
	private static final String SERVER_URL_POST = "http://posttestserver.com/post.php?dump&html&dir=armatus&status_code=202";

	public HermitHttpPostRequest(ConsoleActivity console) {
		super(console);
	}

	@Override
	protected Result doInBackground(String... params) {
		HttpClient client = new DefaultHttpClient();
		HttpPost post = new HttpPost(SERVER_URL_POST);
		String responseStr;

		try {
			List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>();
			nameValuePairs.add(new BasicNameValuePair("Armatus", "is"));
			nameValuePairs.add(new BasicNameValuePair("super", "awesome"));

			post.setEntity(new UrlEncodedFormEntity(nameValuePairs));
			HttpResponse httpResponse = client.execute(post);
			if (!isCancelled()) {
				client.execute(post);
			}

			/* Only use these lines if you want to read the server response */
			BufferedReader rd = new BufferedReader(new InputStreamReader(httpResponse.getEntity().getContent()));

			String line = "";
			while ((line = rd.readLine()) != null) {
				System.out.println(line);
				if (line.startsWith("Auth=")) {
					String key = line.substring(5);
					// Do something with the key
				}
			}

			responseStr = "Success!";
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			responseStr = "Unsupported encoding error.";
		} catch (ClientProtocolException e) {
			e.printStackTrace();
			responseStr = "Client protocol error.";
		} catch (IOException e) {
			e.printStackTrace();
			responseStr = "IO error.";
		}
		
		return onResponse(responseStr);
	}

	protected abstract Result onResponse(String response);

}
