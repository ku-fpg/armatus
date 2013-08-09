package edu.kufpg.armatus.server;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

import edu.kufpg.armatus.AsyncActivityTask;
import edu.kufpg.armatus.console.ConsoleActivity;

import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;

public class HermitBluetoothServer extends AsyncActivityTask<ConsoleActivity, String, String, String> {
	private BluetoothDevice mDevice;
	private BluetoothSocket mSocket;
	private OutputStream mOutStream;
	private InputStream mInStream;

	public HermitBluetoothServer(ConsoleActivity activity, BluetoothDevice device) {
		super(activity);
		mDevice = device;
	}

	@Override
	protected String doInBackground(String... params) {
		publishProgress("Attempting socket creation...");
		try {
			mSocket = mDevice.createRfcommSocketToServiceRecord(BluetoothUtils.BASE_UUID);
			publishProgress("Socket created! Attempting socket connection...");
		} catch (IOException e) {
			e.printStackTrace();
			publishProgress("ERROR: Socket creation failed.");
			return null;
		}

		BluetoothUtils.getBluetoothAdapter(getActivity()).cancelDiscovery();
		try {
			mSocket.connect();
		} catch (IOException e) {
			e.printStackTrace();
			publishProgress("ERROR: Socket connection failed. Ensure that the server is up and try again.");
			return null;
		}

		publishProgress("Attempting to send data to server. Creating output stream...");
		String message = params[0];
		if (message == null) {
			message = "No message provided!";
		}
		byte[] messageBytes = message.getBytes();
		publishProgress("Output stream created! Sending message (" + message + ") to server...");
		try {
			mOutStream = mSocket.getOutputStream();
		} catch (IOException e) {
			e.printStackTrace();
			publishProgress("ERROR: Output stream creation failed. Ensure that the server is up and try again.");
			return null;
		}

		try {
			mOutStream.write(messageBytes);
		} catch (IOException e) {
			e.printStackTrace();
			publishProgress("ERROR: Message sending failed. Ensure that the server is up and try again.");
			return null;
		}

		publishProgress("Message sent! Preparing for server response...");
		try {
			mInStream = mSocket.getInputStream();
		} catch (IOException e) {
			e.printStackTrace();
			publishProgress("ERROR: Input stream creation failed. Ensure that the server is up and try again.");
			return null;
		}
		BufferedReader serverReader = new BufferedReader(new InputStreamReader(mInStream));
		String response = null;
		char[] buffer = new char[5000];
		try {
			/**
			 * WARNING! If the Android device is not connected to the server by this point,
			 * calling read() will crash the app without throwing an exception!
			 */
			serverReader.read(buffer);
			response = new String(buffer);
		} catch (IOException e) {
			e.printStackTrace();
			publishProgress("ERROR: Failed to read server response. Ensure that the server is up and try again.");
			return null;
		}

		return response;
	}

	@Override
	protected void onProgressUpdate(String... progress) {
		getActivity().appendConsoleEntry(progress[0]);
	}

	@Override
	protected void onCancelled() {
		super.onCancelled();
		end();
	}

	@Override
	protected void onPostExecute(String result) {
		super.onPostExecute(result);
		if (result != null) {
			getActivity().appendConsoleEntry("Response from server: " + result);
		}
		end();
	}

	private void end() {
		try {
			if (mOutStream != null) {
				mOutStream.flush();
				mOutStream.close();
			}

			if (mInStream != null) {
				mInStream.close();
			}

			if (mSocket != null) {
				mSocket.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
			publishProgress("ERROR: Closing something failed.");
		}
	}

}