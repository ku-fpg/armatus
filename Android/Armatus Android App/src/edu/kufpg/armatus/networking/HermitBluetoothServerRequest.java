package edu.kufpg.armatus.networking;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;

import edu.kufpg.armatus.command.AsyncCommandTask;
import edu.kufpg.armatus.console.ConsoleActivity;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.bluetooth.BluetoothSocket;

public class HermitBluetoothServerRequest extends AsyncCommandTask<String, String, String> {
	private BluetoothAdapter mAdapter;
	private BluetoothDevice mDevice;
	private BluetoothSocket mSocket;

	public HermitBluetoothServerRequest(ConsoleActivity console) {
		super(console);
	}

	@Override
	protected void onPreExecute() {
		super.onPreExecute();

		getActivity().setProgressBarVisibility(true);
		getActivity().disableInput();
		mAdapter = BluetoothUtils.getBluetoothAdapter(getActivity());
		mDevice = BluetoothUtils.getBluetoothDevice(getActivity());
		mSocket = BluetoothUtils.getBluetoothSocket(getActivity());
	}

	@Override
	protected String doInBackground(String... params) {
		if (mSocket != null && mDevice != null && mAdapter != null) {

			if (!mSocket.isConnected() || BluetoothUtils.lastConnectionFailed()) {
				mAdapter.cancelDiscovery();
				try {
					mSocket.connect();
				} catch (IOException e) {
					e.printStackTrace();
					publishProgress("ERROR: Socket connection failed. Ensure that the server is up and try again.");
					return null;
				}
			}

			publishProgress("Attempting to send data to server. Creating output stream...");
			String message = params[0];
			if (message == null) {
				message = "No message provided!";
			}
			byte[] messageBytes = message.getBytes();
			publishProgress("Output stream created! Sending message (" + message + ") to server...");
			OutputStream outStream = null;
			try {
				outStream = mSocket.getOutputStream();
			} catch (IOException e) {
				e.printStackTrace();
				publishProgress("ERROR: Output stream creation failed. Ensure that the server is up and try again.");
				return null;
			}

			try {
				outStream.write(messageBytes);
			} catch (IOException e) {
				e.printStackTrace();
				publishProgress("ERROR: Message sending failed. Ensure that the server is up and try again.");
				return null;
			}

			publishProgress("Message sent! Preparing for server response...");
			InputStream inStream = null;
			try {
				inStream = mSocket.getInputStream();
			} catch (IOException e) {
				e.printStackTrace();
				publishProgress("ERROR: Input stream creation failed. Ensure that the server is up and try again.");
				return null;
			}
			BufferedReader serverReader = new BufferedReader(new InputStreamReader(inStream));
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

		return null;
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
			BluetoothUtils.notifyLastConnectionSucceeded();
		} else {
			BluetoothUtils.notifyLastConnectionFailed();
		}
		end();
	}

	private void end() {
		getActivity().enableInput();
		getActivity().setProgressBarVisibility(false);
	}

}