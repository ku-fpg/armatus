package edu.kufpg.armatus.console;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnCancelListener;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.os.Message;
import android.os.Parcel;
import android.os.Parcelable;
import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import com.google.common.base.Function;
import com.google.common.base.Optional;
import com.google.common.collect.Collections2;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;
import edu.kufpg.armatus.DeviceConstants;
import edu.kufpg.armatus.Prefs;
import edu.kufpg.armatus.data.Command;
import edu.kufpg.armatus.data.CommandInfo;
import edu.kufpg.armatus.data.CommandResponse;
import edu.kufpg.armatus.data.Complete;
import edu.kufpg.armatus.data.Completion;
import edu.kufpg.armatus.data.History;
import edu.kufpg.armatus.data.HistoryCommand;
import edu.kufpg.armatus.data.Token;
import edu.kufpg.armatus.networking.BluetoothUtils;
import edu.kufpg.armatus.networking.HermitBluetoothReceiveRequest;
import edu.kufpg.armatus.networking.HermitBluetoothSendRequest;
import edu.kufpg.armatus.networking.HermitHttpServerRequest;
import edu.kufpg.armatus.networking.HermitHttpServerRequest.HttpRequest;
import edu.kufpg.armatus.networking.InternetUtils;
import edu.kufpg.armatus.util.JsonUtils;
import edu.kufpg.armatus.util.ParcelUtils;
import edu.kufpg.armatus.util.StringUtils;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.Arrays;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

public class HermitClient implements Parcelable {
    public static int NO_TOKEN = -1;
    private static final String HISTORY_FILENAME = "/history.txt";
    private ConsoleActivity mConsole;
    private ProgressDialog mProgress;

    private RequestName mDelayedRequestName = RequestName.NULL;
    private String mServerUrl;
    private Bundle mTempBundle = new Bundle();
    private Token mToken;
    private final Handler mHandler = new Handler() {
        @Override public void handleMessage(@NonNull final Message msg) {
            final String input = (String) msg.obj;
            if (input != null) {
                mConsole.appendErrorResponse(input);
            }
        }
    };

    public HermitClient(@NonNull final ConsoleActivity console) {
        mConsole = console;
    }

    public void handleInput(@Nullable final String input) {
        mHandler.obtainMessage(-1, input).sendToTarget();
    }

    public void completeInput(@NonNull final String input) {
        if (isTokenAcquired(false)) {
            if (isNetworkConnected(RequestName.COMPLETE)) {
                final Complete complete = new Complete(mToken.getUser(), input);
                newCompleteInputRequest().execute(mServerUrl + "/complete", complete.toString());
            } else {
                mTempBundle.putString("input", input);
            }
        } else {
            mConsole.attemptInputCompletion(null);
        }
    }

    public void connect(@NonNull final String serverUrl) {
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
            newSaveHistoryRequest().execute(mServerUrl + "/history", mToken.toString());
        }
    }

    public void loadHistory() {
        if (isNetworkConnected(RequestName.HISTORY) && isTokenAcquired(false)) {
            final String path;
            if (Prefs.isHistoryDirCustom(mConsole)) {
                path = Prefs.getHistoryDir(mConsole);
            } else {
                path = DeviceConstants.CACHE_DIR;
            }

            final File file = new File(path + HISTORY_FILENAME);
            if (file.exists()) {
                try {
                    final History history = new History(JsonUtils.openJsonFile(file.getAbsolutePath()));
                    loadHistoryCommands(history.getCommands());
                } catch (final FileNotFoundException e) {
                    e.printStackTrace();
                } catch (final JSONException e) {
                    mConsole.appendErrorResponse("ERROR: saved history corrupted.");
                    e.printStackTrace();
                }
            } else {
                mConsole.appendErrorResponse("ERROR: no saved history exists.");
            }
        } else {
            mConsole.appendErrorResponse("ERROR: connect before attempting to load history.");
        }
    }

    public void runCommand(@NonNull final String input, final int charsPerLine) {
        final String[] inputs = input.trim().split(StringUtils.WHITESPACE);
        mConsole.addUserInputEntry(input);
        if (CustomCommandDispatcher.isCustomCommand(inputs[0])) {
            if (inputs.length == 1) {
                CustomCommandDispatcher.runCustomCommand(mConsole, inputs[0]);
            } else {
                CustomCommandDispatcher.runCustomCommand(mConsole, inputs[0],
                        Arrays.copyOfRange(inputs, 1, inputs.length));
            }
        } else {
            //Hardcode Bluetooth testing for now since I'm lazy
            if (Prefs.isBluetoothSource(mConsole)) {
                if (input.equals("btconnect") && !BluetoothUtils.isBluetoothConnected(mConsole)) {
                    new HermitBluetoothReceiveRequest(mConsole).executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR);
                } else if (isNetworkConnected(RequestName.COMMAND) && BluetoothUtils.isBluetoothConnected(mConsole)) {
                    final String cleanInput = StringUtils.noCharWrap(input);
                    new HermitBluetoothSendRequest(mConsole).executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR, cleanInput);
                } else if (BluetoothUtils.isBluetoothConnected(mConsole)) {
                    mTempBundle.putString("input", input);
                    mTempBundle.putInt("charsPerLine", charsPerLine);
                }
            } else {
                if (isNetworkConnected(RequestName.COMMAND) && isTokenAcquired(true)) {
                    final String cleanInput = StringUtils.noCharWrap(input);
                    final Command command = new Command(mToken, cleanInput, charsPerLine);
                    if (inputs[0].equals("abort") || inputs[0].equals("resume")) {
                        newRunAbortResumeRequest().execute(mServerUrl + "/command", command.toString());
                    } else {
                        newRunCommandRequest(cleanInput).execute(mServerUrl + "/command", command.toString());
                    }
                } else {
                    mTempBundle.putString("input", input);
                    mTempBundle.putInt("charsPerLine", charsPerLine);
                }
            }
        }
    }

    @NonNull private HermitHttpServerRequest<List<Completion>> newCompleteInputRequest() {
        return new HermitHttpServerRequest<List<Completion>>(mConsole, HttpRequest.POST) {
            @Override protected void onPreExecute() {
                super.onPreExecute();
                getActivity().disableInput(false);
            }

            @Override protected List<Completion> onResponse(@NonNull final String response) {
                final JSONObject insertNameHere;
                try {
                    insertNameHere = new JSONObject(response);
                    final JSONArray completions = insertNameHere.getJSONArray("completions");
                    final ImmutableList.Builder<Completion> completionsBuilder = ImmutableList.builder();
                    for (int i = 0; i < completions.length(); i++) {
                        completionsBuilder.add(new Completion(completions.getJSONObject(i)));
                    }
                    return completionsBuilder.build();
                } catch (final JSONException e) {
                    e.printStackTrace();
                    return null;
                }
            }

            @Override protected void onCancelled(@Nullable final List<Completion> error) {
                final Optional<String> newErrorMessage = getErrorMessage();
                setErrorMessage(null);
                if (newErrorMessage.isPresent() && getActivity() != null) {
                    getActivity().addErrorResponseEntry(newErrorMessage.get());
                }

                super.onCancelled(error);
            }

            @Override protected void onPostExecute(@Nullable final List<Completion> completions) {
                super.onPostExecute(completions);
                final SortedSet<String> suggestions = new TreeSet<String>();

                if (completions != null) {
                    suggestions.addAll(Collections2.transform(completions, new Function<Completion, String>() {
                        @Override
                        public String apply(final Completion input) {
                            return input.getReplacement();
                        }
                    }));
                    getActivity().attemptInputCompletion(suggestions);
                }
            }
        };
    }

    private HermitHttpServerRequest<Token> newConnectRequest() {
        return new HermitHttpServerRequest<Token>(mConsole, HttpRequest.POST) {

            @Override protected void onPreExecute() {
                super.onPreExecute();

                getActivity().setProgressBarVisibility(false);
                showProgressDialog(getActivity(), this, "Connecting...");
            }

            @Override protected void onActivityDetached() {
                if (mProgress != null) {
                    mProgress.dismiss();
                    mProgress = null;
                }
            }

            @Override protected void onActivityAttached() {
                if (mProgress == null) {
                    showProgressDialog(getActivity(), this, "Connecting...");
                }
            }

            @Override protected Token onResponse(@NonNull final String response) {
                try {
                    return new Token(new JSONObject(response));
                } catch (final JSONException e) {
                    e.printStackTrace();
                    return null;
                }
            }

            @Override protected void onCancelled(@Nullable final Token error) {
                super.onCancelled(error);
                dismissProgressDialog();
            }

            @Override protected void onPostExecute(@Nullable final Token token) {
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
            @Override protected void onPreExecute() {
                super.onPreExecute();

                getActivity().setProgressBarVisibility(false);
                showProgressDialog(getActivity(), this, "Fetching commands...");
            }

            @Override protected void onActivityDetached() {
                if (mProgress != null) {
                    mProgress.dismiss();
                    mProgress = null;
                }
            }

            @Override protected void onActivityAttached() {
                if (mProgress == null) {
                    showProgressDialog(getActivity(), this, "Fetching commands...");
                }
            }

            @Override protected List<CommandInfo> onResponse(@NonNull final String response) {
                final JSONObject insertNameHere;
                try {
                    insertNameHere = new JSONObject(response);
                } catch (final JSONException e) {
                    e.printStackTrace();
                    return null;
                }
                try {
                    final JSONArray cmds = insertNameHere.getJSONArray("cmds");
                    final ImmutableList.Builder<CommandInfo> commandListBuilder = ImmutableList.builder();
                    for (int i = 0; i < cmds.length(); i++) {
                        commandListBuilder.add(new CommandInfo(cmds.getJSONObject(i)));
                    }
                    return commandListBuilder.build();
                } catch (final JSONException e) {
                    e.printStackTrace();
                    return null;
                }
            }

            @Override protected void onCancelled(@Nullable final List<CommandInfo> error) {
                super.onCancelled(error);
                dismissProgressDialog();
            }

            @Override protected void onPostExecute(@Nullable final List<CommandInfo> commands) {
                super.onPostExecute(commands);

                if (commands != null) {
                    final ImmutableSortedSet.Builder<String> tags = ImmutableSortedSet.naturalOrder();
                    final SortedSetMultimap<String, String> tagCommandNames = TreeMultimap.create();
                    final SortedSetMultimap<String, CommandInfo> commandNameInfos = TreeMultimap.create();

                    tags.add(CommandHolder.COMMONLY_USED_COMMANDS_TAG);
                    for (final CommandInfo cmdInfo : commands) {
                        final String cmdName = cmdInfo.getName();
                        if (CommandHolder.isCommonlyUsedCommand(cmdName)) {
                            tagCommandNames.put(CommandHolder.COMMONLY_USED_COMMANDS_TAG, cmdName);
                        }
                        for (final String tag : cmdInfo.getTags()) {
                            tags.add(tag);
                            tagCommandNames.put(tag, cmdName);
                        }
                        commandNameInfos.put(cmdName, cmdInfo);
                    }

                    CommandHolder.setTags(tags.build());
                    CommandHolder.setTagCommandNames(tagCommandNames);
                    CommandHolder.setCommandInfos(commandNameInfos);
                    getActivity().updateCommandExpandableMenu();
                    dismissProgressDialog();
                }
            }

        };
    }

    private HermitHttpServerRequest<Void> newSaveHistoryRequest() {
        return new HermitHttpServerRequest<Void>(mConsole, HttpRequest.POST) {
            @Override protected void onPreExecute() {
                super.onPreExecute();
                getActivity().disableInput(false);
            }

            @Override protected Void onResponse(@NonNull final String response) {
                JSONObject history = null;
                try {
                    history = new JSONObject(response);
                } catch (final JSONException e) {
                    e.printStackTrace();
                }

                final String path;
                if (Prefs.isHistoryDirCustom(getActivity())) {
                    path = Prefs.getHistoryDir(getActivity());
                } else {
                    path = DeviceConstants.CACHE_DIR;
                }

                final File file = new File(path + HISTORY_FILENAME);
                if (file.exists()) {
                    JsonUtils.saveJsonFile(history, file.getAbsolutePath());
                } else {
                    cancel(true);
                }
                return null;
            }

            @Override protected void onCancelled(final Void error) {
                final Optional<String> newErrorMessage = getErrorMessage();
                setErrorMessage(null);
                if (newErrorMessage.isPresent() && getActivity() != null) {
                    getActivity().addErrorResponseEntry(newErrorMessage.get());
                }

                super.onCancelled(error);
            }

            @Override protected void onPostExecute(@Nullable final Void nothing) {
                super.onPostExecute(nothing);
                getActivity().showToast("History saved successfully!");
            }
        };
    }

    private void loadHistoryCommands(@NonNull final List<HistoryCommand> historyCommands) {
        loadHistoryCommands(historyCommands, 0);
    }

    private void loadHistoryCommands(@NonNull final List<HistoryCommand> historyCommands, final int index) {
        if (!historyCommands.isEmpty()) {
            final Command tokenCommand = new Command(mToken, historyCommands.get(index).getCommand());
            new HermitHttpServerRequest<CommandResponse>(mConsole, HttpRequest.POST) {
                @Override
                protected void onPreExecute() {
                    super.onPreExecute();
                    getActivity().disableInput(false);
                }

                @Override
                protected CommandResponse onResponse(@NonNull final String response) {
                    try {
                        return new CommandResponse(new JSONObject(response));
                    } catch (final JSONException e) {
                        e.printStackTrace();
                        return null;
                    }
                }

                @Override
                protected void onCancelled(final CommandResponse error) {
                    final Optional<String> newErrorMessage = getErrorMessage();
                    setErrorMessage(null);
                    if (newErrorMessage.isPresent() && getActivity() != null) {
                        getActivity().addErrorResponseEntry(newErrorMessage.get());
                    }

                    super.onCancelled(error);
                }

                @Override
                protected void onPostExecute(final CommandResponse response) {
                    super.onPostExecute(response);
                    mToken.setAst(response.getAst());
                    if (index < historyCommands.size() - 1) {
                        loadHistoryCommands(historyCommands, index + 1);
                    } else {
                        getActivity().setCommandHistory(historyCommands);
                        getActivity().addErrorResponseEntry("Session loaded successfully!");
                    }
                }
            }.execute(mServerUrl + "/command", tokenCommand.toString());
        }
    }

    @NonNull private HermitHttpServerRequest<CommandResponse> newRunCommandRequest(@NonNull final String command) {
        return new HermitHttpServerRequest<CommandResponse>(mConsole, HttpRequest.POST) {

            @Override protected CommandResponse doInBackground(final String... params) {
                return super.doInBackground(params);
            }

            @Override protected CommandResponse onResponse(@NonNull final String response) {
                try {
                    return new CommandResponse(new JSONObject(response));
                } catch (final JSONException e) {
                    e.printStackTrace();
                    return null;
                }
            }

            @Override protected void onPostExecute(@Nullable final CommandResponse response) {
                super.onPostExecute(response);

                if (response != null) {
                    final int fromAst = mToken.getAst();
                    final int toAst = response.getAst();
                    mToken.setAst(toAst);
                    getActivity().addCommandHistoryEntry(fromAst, command, toAst);
                    getActivity().appendCommandResponse(response);
                }
            }

        };
    }

    @NonNull private HermitHttpServerRequest<String> newRunAbortResumeRequest() {
        return new HermitHttpServerRequest<String>(mConsole, HttpRequest.POST) {
            @Override protected String onResponse(@NonNull final String response) {
                try {
                    return new JSONObject(response).getString("msg");
                } catch (final JSONException e) {
                    e.printStackTrace();
                    return null;
                }
            }

            @Override protected void onPostExecute(@Nullable final String message) {
                super.onPostExecute(message);
                mToken = null;
                getActivity().clearCommandHistory();
                getActivity().appendErrorResponse(message);

                CommandHolder.setTags(null);
                CommandHolder.setTagCommandNames(null);
                CommandHolder.setCommandInfos(null);
                getActivity().updateCommandExpandableMenu();
            }
        };
    }

    public void runDelayedRequest() {
        if (mDelayedRequestName != null) {
            switch (mDelayedRequestName) {
                case COMMAND: {
                    final String input = mTempBundle.getString("input");
                    final int charsPerLine = mTempBundle.getInt("charsPerLine");
                    runCommand(input, charsPerLine);
                    mTempBundle.remove("input");
                    mTempBundle.remove("charsPerLine");
                    break;
                }
                case COMMANDS:
                    fetchCommands();
                    break;
                case COMPLETE:
                    final String input = mTempBundle.getString("input");
                    completeInput(input);
                    mTempBundle.remove("input");
                    break;
                case CONNECT:
                    connect(mServerUrl);
                    break;
                case HISTORY:
                    fetchHistory();
                    break;
                default:
                    break;
            }
        }
    }

    void attachConsole(@NonNull final ConsoleActivity console) {
        mConsole = console;
    }

    private void dismissProgressDialog() {
        if (mProgress != null) {
            mProgress.dismiss();
        }
    }

    public int getAst() {
        return mToken != null ? mToken.getAst() : NO_TOKEN;
    }

    private boolean isNetworkConnected(@NonNull final RequestName name) {
        if (Prefs.isBluetoothSource(mConsole)) {
            if (BluetoothUtils.isBluetoothEnabled(mConsole)) {
                if (BluetoothUtils.getBluetoothDevice(mConsole).isPresent()) {
                    return true;
                } else {
                    notifyDelay(name);
                    BluetoothUtils.findDeviceName(mConsole);
                }
            } else {
                notifyDelay(name);
                BluetoothUtils.enableBluetooth(mConsole);
            }
        } else if (Prefs.isWebSource(mConsole)) {
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
        return mDelayedRequestName != RequestName.NULL;
    }

    public boolean isTokenAcquired() {
        return isTokenAcquired(false);
    }

    private boolean isTokenAcquired(final boolean complainIfNot) {
        if (mToken == null) {
            if (complainIfNot) {
                mConsole.appendErrorResponse("ERROR: No token (connect to server first).");
            }
            return false;
        }
        return true;
    }

    private void notifyDelay(@NonNull final RequestName name) {
        mDelayedRequestName = name;
    }

    public void notifyDelayedRequestFinished() {
        mDelayedRequestName = RequestName.NULL;
    }

    private void showProgressDialog(@NonNull final Context context,
                                    @NonNull final AsyncTask<?, ?, ?> task,
                                    @NonNull final String message) {
        mProgress = new ProgressDialog(context);
        mProgress.setProgressStyle(ProgressDialog.STYLE_SPINNER);
        mProgress.setMessage(message);
        mProgress.setCancelable(true);
        mProgress.setOnCancelListener(new OnCancelListener() {
            @Override public void onCancel(@NonNull final DialogInterface dialog) {
                task.cancel(true);
            }
        });
        mProgress.show();
    }

    private enum RequestName {
        COMMAND, COMMANDS, COMPLETE, CONNECT, HISTORY, NULL
    }

    public static final Parcelable.Creator<HermitClient> CREATOR
            = new Parcelable.Creator<HermitClient>() {
        @Override public HermitClient createFromParcel(@NonNull final Parcel in) {
            return new HermitClient(in);
        }

        @Override public HermitClient[] newArray(final int size) {
            return new HermitClient[size];
        }
    };

    private HermitClient(@NonNull final Parcel in) {
        mDelayedRequestName = ParcelUtils.readEnum(in);
        mServerUrl = in.readString();
        mTempBundle = in.readBundle();
        mToken = in.readParcelable(Token.class.getClassLoader());
    }


    @Override public int describeContents() {
        return 0;
    }

    @Override public void writeToParcel(@NonNull final Parcel dest, final int flags) {
        ParcelUtils.writeEnum(dest, mDelayedRequestName);
        dest.writeString(mServerUrl);
        dest.writeBundle(mTempBundle);
        dest.writeParcelable(mToken, flags);
    }

}
