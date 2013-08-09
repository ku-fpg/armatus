package edu.kufpg.armatus.server;

import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.conn.ConnectTimeoutException;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;
import org.apache.http.params.HttpConnectionParams;
import org.apache.http.params.HttpParams;
import org.json.JSONException;
import org.json.JSONObject;

import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

/**
 * Task that connects to a server running HERMIT-web and simulates HERMIT commands
 * by using HTTP GET and POST requests.
 */
public class HermitWebServer extends AsyncActivityTask<ConsoleActivity, JSONObject, String, String> {
	/** The URL used for performing HTTP GET requests. */
	private static final String SERVER_URL_GET = "https://raw.github.com/flori/json/master/data/example.json";
	
	/** The URL used for performing HTTP POST requests. */
	private static final String SERVER_URL_POST = "http://posttestserver.com/post.php?dump&html&dir=armatus&status_code=202";

	/**
	 * Constructs a new instance. The constructor is not the place to put any input
	 * {@link JSONObject}s (do that in {@link android.os.AsyncTask#execute(JSONObject...)
	 * execute(JSONObject...)} instead).
	 * @param console reference to the current console.
	 */
	public HermitWebServer(ConsoleActivity console) {
		super(console);
	}

	@Override
	protected void onPreExecute() {
		super.onPreExecute();

		getActivity().setProgressBarVisibility(true);
		getActivity().disableInput();
	}

	//WARNING: Do not use getActivity() in doInBackground!
	@Override
	protected String doInBackground(JSONObject... params) {
		return httpGet();
		//return httpPost();
	}

	@Override
	protected void onProgressUpdate(String... progress) {
		if (getActivity() != null) {
			if (progress[0] != null) {
				getActivity().appendConsoleEntry(progress[0]);
			}
		}
	}

	@Override
	protected void onPostExecute(final String response) {
		super.onPostExecute(response);

		if (getActivity() != null) {
			String message = response;
			if (response == null) {
				message = "Error: server request failed to complete.";
			}
			getActivity().appendConsoleEntry(message);
			getActivity().enableInput();
			getActivity().setProgressBarVisibility(false);
		}
	}

	@Override
	protected void onCancelled() {
		super.onCancelled();

		if (getActivity() != null) {
			getActivity().appendConsoleEntry("Error: server request cancelled.");
		}
	}

	/**
	 * Performs an HTTP GET request to {@link #SERVER_URL_GET} and returns the response.
	 * @return the response from the server. If there is no response, an appropriate
	 * error message is instead returned.
	 */
	private String httpGet() {
		HttpResponse httpResponse = null;
		HttpClient client = null;
		String jsonResponse, jsonText = null;

		try {
			final HttpParams httpParams = new BasicHttpParams();
			//Set timeout length to 30 seconds
			HttpConnectionParams.setConnectionTimeout(httpParams, 30000);
			client = new DefaultHttpClient(httpParams);
			HttpGet request = new HttpGet(SERVER_URL_GET);
			if (!isCancelled()) {
				publishProgress("Attempting server connection...");
				httpResponse = client.execute(request);
			}
			publishProgress("Reading in JSON...");
			InputStream jsonIS = httpResponse.getEntity().getContent();
			Scanner jsonScanner = new Scanner(jsonIS).useDelimiter("\\A");
			jsonResponse = jsonScanner.hasNext() ? jsonScanner.next() : "";
			jsonScanner.close();
			if (!isCancelled()) {
				publishProgress("Parsing JSON...");
				JSONObject json = new JSONObject(jsonResponse);
				jsonText = json.toString();
			}
		} catch(ConnectTimeoutException e) {
			e.printStackTrace();
			jsonResponse = "Connection timeout error.";
		} catch(UnknownHostException e) {
			e.printStackTrace();
			jsonResponse = "Unknown host error.";
		} catch(IOException e) {
			e.printStackTrace();
			jsonResponse = "IO error.";
		} catch (JSONException e) {
			e.printStackTrace();
			jsonResponse = "JSON error.";
		} finally {
			client.getConnectionManager().shutdown();
		}

		return jsonText;
	}

	/**
	 * Performs an HTTP POST request and returns the response.
	 * @return the response from the server. If there is no response, an alternative
	 * message (e.g., "Success" or some kind of error message) is instead returned.
	 */
	@SuppressWarnings("unused")
	private String httpPost() {
		HttpClient client = new DefaultHttpClient();
		HttpPost post = new HttpPost(SERVER_URL_POST);

		try {
			List<NameValuePair> nameValuePairs = new ArrayList<NameValuePair>(1);
			nameValuePairs.add(new BasicNameValuePair("Armatus", "is"));
			nameValuePairs.add(new BasicNameValuePair("super", "awesome"));

			post.setEntity(new UrlEncodedFormEntity(nameValuePairs));
			//			HttpResponse response = client.execute(post);
			if (!isCancelled()) {
				client.execute(post);
			}

			/* Only use these lines if you want to read the server response */
			//			BufferedReader rd = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));
			//
			//			String line = "";
			//			while ((line = rd.readLine()) != null) {
			//				System.out.println(line);
			//				if (line.startsWith("Auth=")) {
			//					String key = line.substring(5);
			//					// Do something with the key
			//				}
			//
			//			}

			return "Success!";
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
			return "Unsupported encoding error.";
		} catch (ClientProtocolException e) {
			e.printStackTrace();
			return "Client protocol error.";
		} catch (IOException e) {
			e.printStackTrace();
			return "IO error.";
		}
	}

}
