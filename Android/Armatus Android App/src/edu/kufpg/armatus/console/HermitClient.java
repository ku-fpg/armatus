package edu.kufpg.armatus.console;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.app.ProgressDialog;
import android.content.Context;
import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableSortedSet;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.PrefsActivity;
import edu.kufpg.armatus.command.CustomCommandDispatcher;
import edu.kufpg.armatus.networking.BluetoothUtils;
import edu.kufpg.armatus.networking.HermitHttpServerRequest;
import edu.kufpg.armatus.networking.HermitHttpServerRequest.HttpRequest;
import edu.kufpg.armatus.networking.InternetUtils;
import edu.kufpg.armatus.util.StringUtils;

public class HermitClient implements Parcelable {
	private ConsoleActivity mConsole;
	private ProgressDialog mProgress;

	private RequestName mDelayedRequestName;
	private String mServerUrl;
	private Token mToken;

	public HermitClient(ConsoleActivity console) {
		mConsole = console;
	}

	public void connect(String serverUrl) {
		mServerUrl = serverUrl;
		if (isNetworkConnected(RequestName.CONNECT)) {
			newConnectRequest().execute(mServerUrl + "/connect");
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
			if (isNetworkConnected(RequestName.COMMAND) && isTokenAcquired()) {
				JSONObject o = new JSONObject();
				try {
					o.put("token", mToken.toJSONObject());
					o.put("cmd", StringUtils.withoutCharWrap(input));
				} catch (JSONException e) {
					e.printStackTrace();
				}
				newRunCommandRequest().execute(mServerUrl + "/command", o.toString(), inputs[0]);
			}

		}
	}

	public void fetchCommands() {
		if (isNetworkConnected(RequestName.COMMANDS)) {
			newFetchCommandsRequest().execute(mServerUrl + "/commands");
		}
	}

	public class CommandInfo {
		public final String help, name;
		public final List<String> tags;

		public CommandInfo(String help, String name, List<String> tags) {
			this.help = help;
			this.name = name;
			this.tags = tags;
		}

	}

	public static class CommandResponse implements Serializable {
		private static final long serialVersionUID = 6457446122656522614L;
		public final int ast;
		public final List<Glyph> glyphs;

		public CommandResponse(JSONObject o) throws JSONException {
			this(o.getInt("ast"), listOfGlyphs(o.getJSONArray("glyphs")));
		}

		public CommandResponse(int ast, List<Glyph> glyphs) {
			this.ast = ast;
			this.glyphs = glyphs;
		}

		private static List<Glyph> listOfGlyphs(JSONArray a) {
			List<Glyph> list = new ArrayList<Glyph>();
			for (int i = 0; i < a.length(); i++) {
				try {
					list.add(new Glyph(a.getJSONObject(i)));
				} catch (JSONException e) {
					e.printStackTrace();
				}
			}
			return list;
		}
	}

	public static enum GlyphStyle {
		NORMAL, KEYWORD, SYNTAX, VAR, COERCION, TYPE, LIT, WARNING
	}

	public static class Glyph implements Serializable {
		private static final long serialVersionUID = -7814082698884658726L;
		public final GlyphStyle style;
		public final String text;

		public Glyph(JSONObject o) throws JSONException {
			this(getStyle(o), o.getString("text"));
		}

		public Glyph(GlyphStyle style, String text) {
			this.style = style;
			this.text = text;
		}

		private static GlyphStyle getStyle(JSONObject o) {
			if (o.has("style")) {
				GlyphStyle glyphStyle = null;
				try {
					glyphStyle = GlyphStyle.valueOf(o.getString("style"));
				} catch (JSONException e) {
					e.printStackTrace();
				}

				return glyphStyle;
			} else {
				return GlyphStyle.NORMAL;
			}

		}

	}

	public static class Token implements Serializable {
		private static final long serialVersionUID = -8737445107687286266L;
		public final int user;
		public int ast;

		public Token(JSONObject o) throws JSONException {
			this(o.getInt("user"), o.getInt("ast"));
		}

		public Token(int user, int ast) {
			this.user = user;
			this.ast = ast;
		}

		public JSONObject toJSONObject() {
			JSONObject o = new JSONObject();
			try {
				o.put("user", user);
				o.put("ast", ast);
			} catch (JSONException e) {
				e.printStackTrace();
			}

			return o;
		}
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
					List<CommandInfo> commandList = new ArrayList<CommandInfo>();
					for (int i = 0; i < cmds.length(); i++) {
						JSONObject cmdInfo = cmds.getJSONObject(i);
						String name = cmdInfo.getString("name");
						String help = cmdInfo.getString("help");
						JSONArray tags = cmdInfo.getJSONArray("tags");
						List<String> tagList = new ArrayList<String>();
						for (int j = 0; j < tags.length(); j++) {
							tagList.add(tags.getString(j));
							commandList.add(new CommandInfo(help, name, tagList));
						}
					}

					return commandList;
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
				ImmutableListMultimap.Builder<String, String> tagMapBuilder = ImmutableListMultimap.builder();
				ImmutableSortedSet.Builder<String> commandSetBuilder = ImmutableSortedSet.naturalOrder();
				for (CommandInfo cmdInfo : commands) {
					commandSetBuilder.add(cmdInfo.name);
					for (String tag : cmdInfo.tags) {
						tagSetBuilder.add(tag);
						tagMapBuilder.put(tag, cmdInfo.name);
					}
				}

				getActivity().initCommandRelatedVariables(commandSetBuilder.build(), new ArrayList<String>(tagSetBuilder.build()),
						tagMapBuilder.build());
				dismissProgressDialog();
			}

		};
	}

	private HermitHttpServerRequest<CommandResponse> newRunCommandRequest() {
		return new HermitHttpServerRequest<CommandResponse>(mConsole, HttpRequest.POST) {
			private String mmCommandName;

			@Override
			protected CommandResponse doInBackground(String... params) {
				if (params.length > 2 && params[2] != null && !params[2].isEmpty()) {
					mmCommandName = params[2].split(StringUtils.WHITESPACE)[0];
				}
				return super.doInBackground(params);
			}

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
				mToken.ast = response.ast;
				getActivity().appendCommandResponse(response);
			}

		};
	}

	public void runDelayedRequest() {
		if (mDelayedRequestName != null) {
			switch (mDelayedRequestName) {
			case CONNECT:
				connect(mServerUrl);
				break;
			case COMMAND:
				fetchCommands();
				break;
			case COMMANDS:
				fetchCommands();
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
		return mDelayedRequestName != null;
	}
	
	private boolean isTokenAcquired() {
		if (mToken == null) {
			mConsole.appendErrorResponse("ERROR: No token (connect to server first).");
			return false;
		}
		return true;
	}

	private void notifyDelay(RequestName name) {
		mDelayedRequestName = name;
	}

	public void notifyDelayedRequestFinished() {
		mDelayedRequestName = null;
	}
	
	private void showProgressDialog(Context context, String message) {
		mProgress = new ProgressDialog(context);
		mProgress.setProgressStyle(ProgressDialog.STYLE_SPINNER);
		mProgress.setMessage(message);
		mProgress.setCancelable(false);
		mProgress.show();
	}

	private enum RequestName {
		CONNECT, COMMAND, COMMANDS
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
		mDelayedRequestName = (RequestName) in.readSerializable();
		mServerUrl = in.readString();
		mToken = (Token) in.readSerializable();
	}


	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeSerializable(mDelayedRequestName);
		dest.writeString(mServerUrl);
		dest.writeSerializable(mToken);
	}

}
