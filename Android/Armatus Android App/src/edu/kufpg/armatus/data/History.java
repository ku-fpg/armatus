package edu.kufpg.armatus.data;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableList;

import edu.kufpg.armatus.util.ParcelUtils;

public class History implements Parcelable {
	private static final String CMDS = "cmds", TAGS = "tags";
	
	private final List<HistoryCommand> mCommands;
	private final List<HistoryTag> mTags;

	public History(List<HistoryCommand> commands, List<HistoryTag> tags) {
		this(ImmutableList.copyOf(commands), ImmutableList.copyOf(tags));
	}

	public History(JSONObject o) throws JSONException {
		this(jsonToHistoryCommands(o.getJSONArray(CMDS)), jsonToHistoryTags(o.getJSONArray(TAGS)));
	}

	private History(ImmutableList<HistoryCommand> commands, ImmutableList<HistoryTag> tags) {
		mCommands = commands;
		mTags = tags;
	}

	public List<HistoryCommand> getCommands() {
		return mCommands;
	}

	public List<HistoryTag> getTags() {
		return mTags;
	}

	private static ImmutableList<HistoryCommand> jsonToHistoryCommands(JSONArray a) throws JSONException {
		ImmutableList.Builder<HistoryCommand> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			builder.add(new HistoryCommand(a.getJSONObject(i)));
		}
		return builder.build();
	}

	private static ImmutableList<HistoryTag> jsonToHistoryTags(JSONArray a) throws JSONException {
		ImmutableList.Builder<HistoryTag> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			builder.add(new HistoryTag(a.getJSONObject(i)));
		}
		return builder.build();
	}

	public static Parcelable.Creator<History> CREATOR =
			new Parcelable.Creator<History>() {
		@Override
		public History createFromParcel(Parcel source) {
			ImmutableList<HistoryCommand> commands = ParcelUtils.readImmutableList(source);
			List<HistoryTag> tags = ParcelUtils.readImmutableList(source);
			return new History(commands, tags);
		}

		@Override
		public History[] newArray(int size) {
			return new History[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		ParcelUtils.writeList(dest, mCommands);
		ParcelUtils.writeList(dest, mTags);
	}
}
