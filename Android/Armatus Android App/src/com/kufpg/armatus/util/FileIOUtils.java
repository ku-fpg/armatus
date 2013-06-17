package com.kufpg.armatus.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.lang.ref.WeakReference;

import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpGet;
import org.json.JSONException;
import org.json.JSONObject;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.http.AndroidHttpClient;
import android.os.AsyncTask;
import android.os.Environment;
import android.util.Log;
import android.widget.ImageView;

public class FileIOUtils {
	
	public static final String CACHE_DIR = Environment.getExternalStorageDirectory().getPath() + "/data/armatus/";

	public static void saveJsonFile(JSONObject obj, String path) {
		try {
			File file = new File(path);
			file.getParentFile().mkdirs();
			FileWriter writer = new FileWriter(path);
			writer.write(obj.toString());
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static JSONObject openJsonFile(String path) {	 
		try {	 
			return new JSONObject(openTextFile(path)); 
		} catch (JSONException e) {
			e.printStackTrace();
			return null;
		}
	}

	public static String openTextFile(String path) {
		BufferedReader br = null;
		try {
			br = new BufferedReader(new FileReader(path));
			StringBuilder sb = new StringBuilder();
			String line = br.readLine();

			while (line != null) {
				sb.append(line + "\n");
				line = br.readLine();
			}
			
			br.close();
			return sb.toString();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

	public static void downloadImage(String url, ImageView imageView, boolean showProgress, Context context) {
		BitmapDownloaderTask task = new BitmapDownloaderTask(imageView, showProgress, context);
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
				mDialog.setButton(DialogInterface.BUTTON_NEGATIVE, "Cancel", new OnClickListener() {
					@Override
					public void onClick(DialogInterface dialog, int which) {
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
			final AndroidHttpClient client = AndroidHttpClient.newInstance("Android");
			final HttpGet getRequest = new HttpGet(url);

			try {
				HttpResponse response = client.execute(getRequest);
				final int statusCode = response.getStatusLine().getStatusCode();
				if (statusCode != HttpStatus.SC_OK) {
					Log.w("ImageDownloader", "Error " + statusCode + " while retrieving bitmap from " + url);
					return null;
				}

				final HttpEntity entity = response.getEntity();
				if (entity != null) {
					InputStream inputStream = null;
					try {
						inputStream = entity.getContent();
						final Bitmap bitmap = BitmapFactory.decodeStream(inputStream);
						return bitmap;
					} finally {
						if (inputStream != null) {
							inputStream.close();
						}
						entity.consumeContent();
					}
				}
			} catch (Exception e) {
				// Could provide a more explicit error message for IOException or IllegalStateException
				getRequest.abort();
				Log.w("ImageDownloader", "Error " + e.toString() + " while retrieving bitmap from " + url);
			} finally {
				if (client != null) {
					client.close();
				}
			}
			return null;
		}
	}

}