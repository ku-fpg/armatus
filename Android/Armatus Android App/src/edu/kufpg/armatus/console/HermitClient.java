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
import android.graphics.Color;
import android.os.Parcel;
import android.os.Parcelable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableSortedSet;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.PrefsActivity;
import edu.kufpg.armatus.networking.BluetoothUtils;
import edu.kufpg.armatus.networking.HermitHttpServerRequest;
import edu.kufpg.armatus.networking.HermitHttpServerRequest.HttpRequest;
import edu.kufpg.armatus.networking.InternetUtils;
import edu.kufpg.armatus.util.StringUtils;

public class HermitClient implements Parcelable {
	public static int NO_TOKEN = -1;

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

				if (inputs[0].equals("abort") || inputs[0].equals("resume")) {
					newRunAbortResumeRequest().execute(mServerUrl + "/command", o.toString());
				} else {
					newRunCommandRequest().execute(mServerUrl + "/command", o.toString());
				}
			}

		}
	}

	public void fetchCommands() {
		if (isNetworkConnected(RequestName.COMMANDS)) {
			newFetchCommandsRequest().execute(mServerUrl + "/commands");
		}
	}

	public static class CommandInfo implements Parcelable {
		private final String mHelp, mName;
		private final List<String> mTags;

		public CommandInfo(String help, String name, List<String> tags) {
			mHelp = help;
			mName = name;
			mTags = ImmutableList.copyOf(tags);
		}

		public String getHelp() {
			return mHelp;
		}

		public String getName() {
			return mName;
		}

		public List<String> getTags() {
			return mTags;
		}

		public static Parcelable.Creator<CommandInfo> CREATOR =
				new Parcelable.Creator<CommandInfo>() {
			@Override
			public CommandInfo createFromParcel(Parcel source) {
				String help = source.readString();
				String name = source.readString();
				@SuppressWarnings("unchecked")
				List<String> tags = (List<String>) source.readSerializable();

				return new CommandInfo(help, name, tags);
			}

			@Override
			public CommandInfo[] newArray(int size) {
				return new CommandInfo[size];
			}
		};

		@Override
		public int describeContents() {
			return 0;
		}

		@Override
		public void writeToParcel(Parcel dest, int flags) {
			dest.writeString(mHelp);
			dest.writeString(mName);
			dest.writeSerializable((Serializable) mTags);
		}

	}

	public static class CommandResponse implements Serializable {
		private static final long serialVersionUID = 6457446122656522614L;
		private static final String BLUE = "#0090D3";
		private static final String RED = "#CC060B";
		private static final String YELLOW = "#FDFD0D";
		private static final String GREEN = "#1DDA1C";
		private static final String CYAN = "#1BE0CC";

		private final int mAst;
		private final List<Glyph> mGlyphs;

		public CommandResponse(JSONObject o) throws JSONException {
			this(o.getInt("ast"), jsonToGlyphs(o.getJSONArray("glyphs")));
		}

		public CommandResponse(int ast, List<Glyph> glyphs) {
			mAst = ast;
			mGlyphs = glyphs;
		}

		public CharSequence createPrettyText() {
			SpannableStringBuilder builder = new SpannableStringBuilder();
			for (Glyph glyph : mGlyphs) {
				SpannableString spanWord = new SpannableString(glyph.getText());
				if (glyph.getStyle().equals(GlyphStyle.WARNING)) {
					spanWord.setSpan(new BackgroundColorSpan(Color.YELLOW),
							0, glyph.getText().length(), 0);
					spanWord.setSpan(new ForegroundColorSpan(Color.BLACK),
							0, glyph.getText().length(), 0);
				} else {
					String glyphColor = getGlyphColor(glyph.getStyle());
					if (glyphColor != null) {
						spanWord.setSpan(new ForegroundColorSpan(Color.parseColor(glyphColor)),
								0, glyph.getText().length(), 0);
					}

				}
				builder.append(spanWord);
			}
			return builder;
		}

		public int getAst() {
			return mAst;
		}

		public List<Glyph> getGlyphs() {
			return mGlyphs;
		}

		private static String getGlyphColor(GlyphStyle style) {
			switch (style) {
			case KEYWORD:
				return BLUE;
			case SYNTAX:
				return RED;
			case COERCION:
				return YELLOW;
			case TYPE:
				return GREEN;
			case LIT:
				return CYAN;
			default:
				return null;
			}
		}

		private static List<Glyph> jsonToGlyphs(JSONArray a) {
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

	public static class Glyph implements Serializable {
		private static final long serialVersionUID = -7814082698884658726L;

		private final GlyphStyle mStyle;
		private final String mText;

		public Glyph(JSONObject o) throws JSONException {
			this(jsonToStyle(o), o.getString("text"));
		}

		public Glyph(GlyphStyle style, String text) {
			mStyle = style;
			mText = text;
		}

		public GlyphStyle getStyle() {
			return mStyle;
		}

		public String getText() {
			return mText;
		}

		private static GlyphStyle jsonToStyle(JSONObject o) {
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

	public static enum GlyphStyle {
		NORMAL, KEYWORD, SYNTAX, VAR, COERCION, TYPE, LIT, WARNING
	}

	public static class Token implements Serializable {
		private static final long serialVersionUID = -8737445107687286266L;
		private final int mUser;
		private int mAst;

		public Token(JSONObject o) throws JSONException {
			this(o.getInt("user"), o.getInt("ast"));
		}

		public Token(int user, int ast) {
			mUser = user;
			mAst = ast;
		}

		public int getUser() {
			return mUser;
		}

		public int getAst() {
			return mAst;
		}

		public void setAst(int ast) {
			mAst = ast;
		}

		public JSONObject toJSONObject() {
			JSONObject o = new JSONObject();
			try {
				o.put("user", mUser);
				o.put("ast", mAst);
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
				ImmutableListMultimap.Builder<String, CommandInfo> tagMapBuilder = ImmutableListMultimap.builder();
				ImmutableSortedSet.Builder<String> commandSetBuilder = ImmutableSortedSet.naturalOrder();

				for (CommandInfo cmdInfo : commands) {
					commandSetBuilder.add(cmdInfo.getName());
					for (String tag : cmdInfo.getTags()) {
						tagSetBuilder.add(tag);
						tagMapBuilder.put(tag, cmdInfo);
					}
				}

				Commands.setCommandSet(commandSetBuilder.build());
				Commands.setTagList(ImmutableList.copyOf(tagSetBuilder.build()));
				Commands.setTagMap(tagMapBuilder.build());
				getActivity().getWordCompleter().resetFilter(getActivity().getInput());
				getActivity().updateCommandExpandableMenu();
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
			protected void onPostExecute(String response) {
				super.onPostExecute(response);
				mToken = null;
				getActivity().appendErrorResponse(response);
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
