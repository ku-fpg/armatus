package edu.kufpg.armatus.console;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.app.ProgressDialog;
import android.content.Context;

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

public class HermitClient {
	private static final String EXAMPLE_URL = "http://10.92.78.212:3000";

	private ConsoleActivity mConsole;
	private RequestName mDelayedRequestName;
	private ProgressDialog mProgress;

	public HermitClient(ConsoleActivity console) {
		mConsole = console;
	}

	private boolean isConnected(ConsoleActivity console, RequestName name) {
		String server = PrefsActivity.getPrefs(console).getString(
				BaseActivity.NETWORK_SOURCE_KEY, null);
		if (BaseActivity.NETWORK_SOURCE_BLUETOOTH_SERVER.equals(server)) {
			if (BluetoothUtils.isBluetoothEnabled(console)) {
				if (BluetoothUtils.getBluetoothDevice(console) != null) {
					return true;
				} else {
					notifyDelay(name);
					BluetoothUtils.findDeviceName(console);
				}
			} else {
				notifyDelay(name);
				BluetoothUtils.enableBluetooth(console);
			}
		} else if (BaseActivity.NETWORK_SOURCE_WEB_SERVER.equals(server)) {
			if (InternetUtils.isAirplaneModeOn(console)) {
				console.appendErrorResponse("ERROR: Please disable airplane mode before attempting to connect.");
			} else if (!InternetUtils.isWifiConnected(console)
					&& !InternetUtils.isMobileConnected(console)) {
				notifyDelay(name);
				InternetUtils.enableWifi(console);
			} else {
				return true;
			}
		}
		return false;
	}

	public void connect() {
		if (isConnected(mConsole, RequestName.CONNECT)) {
			newConnectRequest().execute(EXAMPLE_URL + "/connect");
		}
	}

	public void runCommand(String input) {
		String[] inputs = input.trim().split(StringUtils.WHITESPACE);
		if (CustomCommandDispatcher.isCustomCommand(inputs[0])) {
			if (inputs.length == 1) {
				CustomCommandDispatcher.runCustomCommand(mConsole, inputs[0]);
			} else {
				CustomCommandDispatcher.runCustomCommand(mConsole, inputs[0],
						Arrays.copyOfRange(inputs, 1, inputs.length));
			}
		} else {
			if (isConnected(mConsole, RequestName.COMMAND)) {
				JSONObject o = new JSONObject();
				try {
					o.put("token", new Token(0,0).toJSONObject());
					o.put("cmd", input);
				} catch (JSONException e) {
					e.printStackTrace();
				}
				
				newRunCommandRequest().execute(EXAMPLE_URL + "/command", o.toString());
			}

		}
	}

	public void fetchCommands() {
		if (isConnected(mConsole, RequestName.COMMANDS)) {
			newFetchCommandsRequest().execute(EXAMPLE_URL + "/commands");
		}
	}

	public class CommandInfo {
		public final String name, help;
		public final List<String> tags;

		public CommandInfo(String name, String help, List<String> tags) {
			this.name = name;
			this.help = help;
			this.tags = tags;
		}

	}

	public static List<Glyph> listOfGlyphs(JSONArray a) {
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

	public static class CommandResponse {
		public final Token token;
		public final List<Glyph> glyphs;

		public CommandResponse(JSONObject o) throws JSONException {
			this(new Token(o.getJSONObject("token")), listOfGlyphs(o
					.getJSONArray("glyphs")));
		}

		public CommandResponse(Token token, List<Glyph> glyphs) {
			this.token = token;
			this.glyphs = glyphs;
		}
	}

	public static enum GlyphStyle {
		NORMAL, KEYWORD, SYNTAX, VAR, TYPE, LIT
	}

	public static GlyphStyle getStyle(JSONObject o) {
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

	public static class Glyph {
		public final String text;
		public final GlyphStyle style;

		public Glyph(JSONObject o) throws JSONException {
			this(o.getString("text"), getStyle(o));
		}

		public Glyph(String text, GlyphStyle style) {
			this.text = text;
			this.style = style;
		}

	}

	public static class Token {
		public final int unique, token;

		public Token(JSONObject o) throws JSONException {
			this(o.getInt("unique"), o.getInt("token"));
		}

		public Token(int unique, int token) {
			this.unique = unique;
			this.token = token;
		}

		public JSONObject toJSONObject() {
			JSONObject o = new JSONObject();
			try {
				o.put("unique", unique);
				o.put("token", token);
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
							commandList.add(new CommandInfo(name, help, tagList));
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
				getActivity().appendCommandResponse(response);
			}

		};
	}

	public void runDelayedRequest() {
		if (mDelayedRequestName != null) {
			switch (mDelayedRequestName) {
			case CONNECT:
				connect();
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

	private void showProgressDialog(Context context, String message) {
		mProgress = new ProgressDialog(context);
		mProgress.setProgressStyle(ProgressDialog.STYLE_SPINNER);
		mProgress.setMessage(message);
		mProgress.setCancelable(false);
		mProgress.show();
	}

	private void dismissProgressDialog() {
		if (mProgress != null) {
			mProgress.dismiss();
		}
	}


	private void notifyDelay(RequestName name) {
		mDelayedRequestName = name;
	}

	public void notifyDelayedRequestFinished() {
		mDelayedRequestName = null;
	}

	public boolean isRequestDelayed() {
		return mDelayedRequestName != null;
	}

	private enum RequestName {
		CONNECT, COMMAND, COMMANDS
	};

}
