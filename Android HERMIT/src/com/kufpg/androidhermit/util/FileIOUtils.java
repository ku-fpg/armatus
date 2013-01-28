package com.kufpg.androidhermit.util;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.lang.ref.WeakReference;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.util.ArrayList;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpGet;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.http.AndroidHttpClient;
import android.os.AsyncTask;
import android.util.Log;
import android.widget.ImageView;

public class FileIOUtils {

	public static ArrayList<String> getTextArrayFromDisk(InputStream textStream) {
		ArrayList<String> textArray = new ArrayList<String>();
		try {
			FileInputStream fileIS = (FileInputStream) textStream;
			BufferedReader buf = new BufferedReader(new InputStreamReader(
					fileIS));
			String readString;
			while ((readString = buf.readLine()) != null) {
				textArray.add(readString);
			}
			buf.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return textArray;
	}

	public static String getTextFromDisk(InputStream textStream) {
		String text = "";
		try {
			FileInputStream fileIS = (FileInputStream) textStream;
			BufferedReader buf = new BufferedReader(new InputStreamReader(
					fileIS));
			String readString;
			while ((readString = buf.readLine()) != null) {
				text += readString + "\n";
			}
			buf.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return text;
	}

	public static ArrayList<String> getTextArrayFromUrl(String textUrlLoc) {
		ArrayList<String> textArray = new ArrayList<String>();
		try {
			URL textUrl = new URL(textUrlLoc);
			BufferedReader buf = new BufferedReader(new InputStreamReader(
					textUrl.openStream()));
			String readString;
			while ((readString = buf.readLine()) != null) {
				textArray.add(readString);
			}
			buf.close();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return textArray;
	}

	public static boolean saveTextArray(ArrayList<String> textArray,
			String path, String fileName) {
		FileWriter writer = null;
		try {
			writer = new FileWriter(path + "/" + fileName);
			for (String str : textArray) {
				writer.write(str + System.getProperty("line.separator"));
			}
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		} finally {
			try {
				writer.close();
				return true;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return false;
	}

	public static boolean isTextFile(String urlLoc) {
		URL url = null;
		try {
			url = new URL(urlLoc);
			HttpURLConnection urlc = (HttpURLConnection) url.openConnection();
			urlc.setAllowUserInteraction(false);
			urlc.setDoInput(true);
			urlc.setDoOutput(false);
			urlc.setUseCaches(true);
			urlc.setRequestMethod("HEAD");
			urlc.connect();
			String mime = urlc.getContentType();
			if (mime != null) {
				mime = mime.replaceAll(";[^;]*$", ""); // Get rid of some extra
														// stuff after the MIME
														// type
				if (mime.equals("text/plain")) {
					return true;
				}
			}
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (ProtocolException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

	public static void downloadImage(String url, ImageView imageView,
			boolean showProgress, Context context) {
		BitmapDownloaderTask task = new BitmapDownloaderTask(imageView,
				showProgress, context);
		task.execute(url);
	}

	public static class BitmapDownloaderTask extends
			AsyncTask<String, Long, Bitmap> {
		private final WeakReference<ImageView> imageViewReference;
		private final boolean mShowProgress;
		private ProgressDialog mDialog;

		public BitmapDownloaderTask(ImageView imageView, boolean showProgress,
				Context context) {
			imageViewReference = new WeakReference<ImageView>(imageView);
			mShowProgress = showProgress;

			if (mShowProgress) {
				mDialog = new ProgressDialog(context);
				mDialog.setTitle("Downloading image");
				mDialog.setMessage("Please wait...");
				mDialog.setCancelable(false);
				mDialog.setButton(DialogInterface.BUTTON_NEGATIVE, "Cancel",
						new OnClickListener() {
							@Override
							public void onClick(DialogInterface dialog,
									int which) {
								BitmapDownloaderTask.this.cancel(true);
							}
						});
				mDialog.show();
			}
		}

		@Override
		// Actual download method, run in the task thread
		protected Bitmap doInBackground(String... params) {
			// params comes from the execute() call: params[0] is the url.
			return downloadBitmap(params[0]);
		}

		@Override
		// Once the image is downloaded, associates it to the imageView
		protected void onPostExecute(Bitmap bitmap) {
			if (isCancelled()) {
				bitmap = null;
			}
			if (imageViewReference != null) {
				ImageView imageView = imageViewReference.get();
				if (imageView != null) {
					imageView.setImageBitmap(bitmap);
				}
			}
			if (mShowProgress) {
				mDialog.dismiss();
			}
		}

		public Bitmap downloadBitmap(String url) {
			final AndroidHttpClient client = AndroidHttpClient
					.newInstance("Android");
			final HttpGet getRequest = new HttpGet(url);

			try {
				HttpResponse response = client.execute(getRequest);
				final int statusCode = response.getStatusLine().getStatusCode();
				if (statusCode != HttpStatus.SC_OK) {
					Log.w("ImageDownloader", "Error " + statusCode
							+ " while retrieving bitmap from " + url);
					return null;
				}

				final HttpEntity entity = response.getEntity();
				if (entity != null) {
					InputStream inputStream = null;
					try {
						inputStream = entity.getContent();
						final Bitmap bitmap = BitmapFactory
								.decodeStream(inputStream);
						return bitmap;
					} finally {
						if (inputStream != null) {
							inputStream.close();
						}
						entity.consumeContent();
					}
				}
			} catch (Exception e) {
				// Could provide a more explicit error message for IOException
				// or IllegalStateException
				getRequest.abort();
				Log.w("ImageDownloader", "Error " + e.toString()
						+ " while retrieving bitmap from " + url);
			} finally {
				if (client != null) {
					client.close();
				}
			}
			return null;
		}
	}

}