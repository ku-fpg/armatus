package edu.kufpg.armatus.console;

import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.app.ProgressDialog;
import android.content.Context;
import android.os.Bundle;
import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.base.Function;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableSortedSet;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.PrefsActivity;
import edu.kufpg.armatus.networking.BluetoothUtils;
import edu.kufpg.armatus.networking.HermitHttpServerRequest;
import edu.kufpg.armatus.networking.HermitHttpServerRequest.HttpRequest;
import edu.kufpg.armatus.networking.InternetUtils;
import edu.kufpg.armatus.networking.data.Command;
import edu.kufpg.armatus.networking.data.CommandInfo;
import edu.kufpg.armatus.networking.data.CommandResponse;
import edu.kufpg.armatus.networking.data.Complete;
import edu.kufpg.armatus.networking.data.Completion;
import edu.kufpg.armatus.networking.data.History;
import edu.kufpg.armatus.networking.data.Token;
import edu.kufpg.armatus.util.StringUtils;

public class HermitClient implements Parcelable {
	public static int NO_TOKEN = -1;

	private ConsoleActivity mConsole;
	private ProgressDialog mProgress;

	private RequestName mDelayedRequestName = RequestName.NULL;
	private String mServerUrl;
	private Bundle mTempBundle = new Bundle();
	private Token mToken;

	public HermitClient(ConsoleActivity console) {
		mConsole = console;
	}

	public void completeInput(final String input) {
		if (isTokenAcquired(false)) {
			if (isNetworkConnected(RequestName.COMPLETE)) {
				Complete complete = new Complete(mToken.getUser(), input);
				newCompleteInputRequest().execute(mServerUrl + "/complete", complete.toString());
			} else {
				mTempBundle.putString("input", input);
			}
		} else {
			mConsole.attemptInputCompletion(null);
		}
	}

	public void connect(String serverUrl) {
		mServerUrl = serverUrl;
		if (isNetworkConnected(RequestName.CONNECT)) {
			newConnectRequest().execute(mServerUrl + "/connect");
		}
	}

	public void fetchCommands() {
		if (isNetworkConnected(RequestName.COMMANDS) && isTokenAcquired(true)) {
			newFetchCommandsRequest().execute(mServerUrl + "/commands");
		}
	}

	public void fetchHistory() {
		if (isNetworkConnected(RequestName.HISTORY) && isTokenAcquired(true)) {
			newFetchHistoryRequest().execute(mServerUrl + "/history", mToken.toString());
		}
	}

	public void runCommand(String input) {
		String[] inputs = input.trim().split(StringUtils.WHITESPACE);
		mConsole.addConsoleUserInputEntry(input);
		if (CustomCommandDispatcher.isCustomCommand(inputs[0])) {
			if (inputs.length == 1) {
				CustomCommandDispatcher.runCustomCommand(mConsole, inputs[0]);
			} else {
				CustomCommandDispatcher.runCustomCommand(mConsole, inputs[0],
						Arrays.copyOfRange(inputs, 1, inputs.length));
			}
		} else {
			if (isNetworkConnected(RequestName.COMMAND) && isTokenAcquired(true)) {
				Command command = new Command(mToken, StringUtils.withoutCharWrap(input));
				if (inputs[0].equals("abort") || inputs[0].equals("resume")) {
					newRunAbortResumeRequest().execute(mServerUrl + "/command", command.toString());
				} else {
					newRunCommandRequest().execute(mServerUrl + "/command", command.toString());
				}
			} else {
				mTempBundle.putString("input", input);
			}
		}
	}

	private HermitHttpServerRequest<List<Completion>> newCompleteInputRequest() {
		return new HermitHttpServerRequest<List<Completion>>(mConsole, HttpRequest.POST) {
			@Override
			protected void onPreExecute() {
				super.onPreExecute();
				getActivity().disableInput(false);
			}
			
			@Override
			protected List<Completion> onResponse(String response) {
				JSONObject insertNameHere = null;
				try {
					insertNameHere = new JSONObject(response);
				} catch (JSONException e) {
					e.printStackTrace();
					return null;
				}
				try {
					JSONArray completions = insertNameHere.getJSONArray("completions");
					ImmutableList.Builder<Completion> completionsBuilder = ImmutableList.builder();
					for (int i = 0; i < completions.length(); i++) {
						completionsBuilder.add(new Completion(completions.getJSONObject(i)));
					}
					return completionsBuilder.build();
				} catch (JSONException e) {
					e.printStackTrace();
					return null;
				}
			}

			@Override
			protected void onPostExecute(List<Completion> completions) {
				super.onPostExecute(completions);
				SortedSet<String> suggestions = new TreeSet<String>();
				suggestions.addAll(Collections2.transform(completions, new Function<Completion, String>() {
					@Override
					public String apply(Completion input) {
						return input.getReplacement();
					}
				}));
				getActivity().attemptInputCompletion(suggestions);
			}
		};
	}

	private HermitHttpServerRequest<Token> newConnectRequest() {
		return new HermitHttpServerRequest<Token>(mConsole, HttpRequest.POST) {

			@Override
			protected void onPreExecute() {
				super.onPreExecute();

				getActivity().setProgressBarVisibility(false);
				showProgressDialog(getActivity(), "Connecting...");
			}

			@Override
			protected void onActivityDetached() {
				if (mProgress != null) {
					mProgress.dismiss();
					mProgress = null;
				}
			}

			@Override
			protected void onActivityAttached() {
				if (mProgress == null) {
					showProgressDialog(getActivity(), "Connecting...");
				}
			}

			@Override
			protected Token onResponse(String response) {
				try {
					return new Token(new JSONObject(response));
				} catch (JSONException e) {
					e.printStackTrace();
					return null;
				}
			}

			@Override
			protected void onCancelled() {
				super.onCancelled();
				dismissProgressDialog();
			}

			@Override
			protected void onPostExecute(Token token) {
				super.onPostExecute(token);
				mToken = token;
				dismissProgressDialog();
				getActivity().updateInput();
				fetchCommands();
			}

		};
	}

	private HermitHttpServerRequest<List<CommandInfo>> newFetchCommandsRequest() {
		return new HermitHttpServerRequest<List<CommandInfo>>(mConsole, HttpRequest.GET) {
			@Override
			protected void onPreExecute() {
				super.onPreExecute();

				getActivity().setProgressBarVisibility(false);
				showProgressDialog(getActivity(), "Fetching commands...");
			}

			@Override
			protected void onActivityDetached() {
				if (mProgress != null) {
					mProgress.dismiss();
					mProgress = null;
				}
			}

			@Override
			protected void onActivityAttached() {
				if (mProgress == null) {
					showProgressDialog(getActivity(), "Fetching commands...");
				}
			}

			@Override
			protected List<CommandInfo> onResponse(String response) {
				JSONObject insertNameHere = null;
				try {
					insertNameHere = new JSONObject(response);
				} catch (JSONException e) {
					e.printStackTrace();
					return null;
				}
				try {
					JSONArray cmds = insertNameHere.getJSONArray("cmds");
					ImmutableList.Builder<CommandInfo> commandListBuilder = ImmutableList.builder();
					for (int i = 0; i < cmds.length(); i++) {
						commandListBuilder.add(new CommandInfo(cmds.getJSONObject(i)));
					}
					return commandListBuilder.build();
				} catch (JSONException e) {
					e.printStackTrace();
					return null;
				}
			}

			@Override
			protected void onCancelled() {
				super.onCancelled();
				dismissProgressDialog();
			}

			@Override
			protected void onPostExecute(List<CommandInfo> commands) {
				super.onPostExecute(commands);
				ImmutableSortedSet.Builder<String> tagSetBuilder = ImmutableSortedSet.naturalOrder();
				ImmutableListMultimap.Builder<String, CommandInfo> tagMapBuilder = ImmutableListMultimap.builder();

				for (CommandInfo cmdInfo : commands) {
					for (String tag : cmdInfo.getTags()) {
						tagSetBuilder.add(tag);
						tagMapBuilder.put(tag, cmdInfo);
					}
				}

				CommandHolder.setTagList(ImmutableList.copyOf(tagSetBuilder.build()));
				CommandHolder.setTagMap(tagMapBuilder.build());
				getActivity().updateCommandExpandableMenu();
				dismissProgressDialog();
			}

		};
	}

	private HermitHttpServerRequest<History> newFetchHistoryRequest() {
		return new HermitHttpServerRequest<History>(mConsole, HttpRequest.GET) {
			@Override
			protected History onResponse(String response) {
				try {
					return new History(new JSONObject(response));
				} catch (JSONException e) {
					e.printStackTrace();
					return null;
				}
			}

			@Override
			protected void onPostExecute(History history) {
				super.onPostExecute(history);
				//Do cool stuff
			}
		};
	}

	private HermitHttpServerRequest<CommandResponse> newRunCommandRequest() {
		return new HermitHttpServerRequest<CommandResponse>(mConsole, HttpRequest.POST) {
			@Override
			protected CommandResponse onResponse(String response) {
				try {
					return new CommandResponse(new JSONObject(response));
				} catch (JSONException e) {
					e.printStackTrace();
					return null;
				}
			}

			@Override
			protected void onPostExecute(CommandResponse response) {
				super.onPostExecute(response);
				mToken.setAst(response.getAst());
				getActivity().appendCommandResponse(response);
			}

		};
	}

	private HermitHttpServerRequest<String> newRunAbortResumeRequest() {
		return new HermitHttpServerRequest<String>(mConsole, HttpRequest.POST) {
			@Override
			protected String onResponse(String response) {
				try {
					return new JSONObject(response).getString("msg");
				} catch (JSONException e) {
					e.printStackTrace();
					return null;
				}
			}

			@Override
			protected void onPostExecute(String message) {
				super.onPostExecute(message);
				mToken = null;
				getActivity().appendErrorResponse(message);
			}

		};
	}

	public void runDelayedRequest() {
		if (mDelayedRequestName != null) {
			switch (mDelayedRequestName) {
			case COMMAND: {
				String input = mTempBundle.getString("input");
				runCommand(input);
				mTempBundle.remove("input");
				break;
			}
			case COMMANDS: {
				fetchCommands();
				break;
			}
			case COMPLETE: {
				String input = mTempBundle.getString("input");
				completeInput(input);
				mTempBundle.remove("input");
				break;
			}
			case CONNECT: {
				connect(mServerUrl);
				break;
			}
			case HISTORY: {
				fetchHistory();
				break;
			}
			default:
				break;
			}
		}
	}

	void attachConsole(ConsoleActivity console) {
		mConsole = console;
	}

	private void dismissProgressDialog() {
		if (mProgress != null) {
			mProgress.dismiss();
		}
	}

	public int getAst() {
		return (mToken != null) ? mToken.getAst() : NO_TOKEN;
	}

	private boolean isNetworkConnected(RequestName name) {
		String server = PrefsActivity.getPrefs(mConsole).getString(
				BaseActivity.NETWORK_SOURCE_KEY, null);
		if (BaseActivity.NETWORK_SOURCE_BLUETOOTH_SERVER.equals(server)) {
			if (BluetoothUtils.isBluetoothEnabled(mConsole)) {
				if (BluetoothUtils.getBluetoothDevice(mConsole) != null) {
					return true;
				} else {
					notifyDelay(name);
					BluetoothUtils.findDeviceName(mConsole);
				}
			} else {
				notifyDelay(name);
				BluetoothUtils.enableBluetooth(mConsole);
			}
		} else if (BaseActivity.NETWORK_SOURCE_WEB_SERVER.equals(server)) {
			if (InternetUtils.isAirplaneModeOn(mConsole)) {
				mConsole.appendErrorResponse("ERROR: Please disable airplane mode before attempting to connect.");
			} else if (!InternetUtils.isWifiConnected(mConsole)
					&& !InternetUtils.isMobileConnected(mConsole)) {
				notifyDelay(name);
				InternetUtils.enableWifi(mConsole);
			} else {
				return true;
			}
		}
		return false;
	}

	public boolean isRequestDelayed() {
		return !mDelayedRequestName.equals(RequestName.NULL);
	}

	private boolean isTokenAcquired(boolean complainIfNot) {
		if (mToken == null) {
			if (complainIfNot) {
				mConsole.appendErrorResponse("ERROR: No token (connect to server first).");
			}
			return false;
		}
		return true;
	}

	private void notifyDelay(RequestName name) {
		mDelayedRequestName = name;
	}

	public void notifyDelayedRequestFinished() {
		mDelayedRequestName = RequestName.NULL;
	}

	private void showProgressDialog(Context context, String message) {
		mProgress = new ProgressDialog(context);
		mProgress.setProgressStyle(ProgressDialog.STYLE_SPINNER);
		mProgress.setMessage(message);
		mProgress.setCancelable(false);
		mProgress.show();
	}

	private enum RequestName {
		COMMAND, COMMANDS, COMPLETE, CONNECT, HISTORY, NULL
	};

	public static final Parcelable.Creator<HermitClient> CREATOR
	= new Parcelable.Creator<HermitClient>() {
		public HermitClient createFromParcel(Parcel in) {
			return new HermitClient(in);
		}

		public HermitClient[] newArray(int size) {
			return new HermitClient[size];
		}
	};

	private HermitClient(Parcel in) {
		mDelayedRequestName = RequestName.values()[in.readInt()];
		mServerUrl = in.readString();
		mTempBundle = in.readBundle();
		mToken = in.readParcelable(HermitClient.class.getClassLoader());
	}


	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(mDelayedRequestName.ordinal());
		dest.writeString(mServerUrl);
		dest.writeBundle(mTempBundle);
		dest.writeParcelable(mToken, flags);
	}

}
