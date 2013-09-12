package edu.kufpg.armatus.networking.data;

import java.io.Serializable;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableList;

public class History implements Parcelable {
	private final List<HistoryCommand> mCommands;
	private final List<HistoryTag> mTags;
	
	public History(List<HistoryCommand> commands, List<HistoryTag> tags) {
		mCommands = commands;
		mTags = tags;
	}
	
	public History(JSONObject o) throws JSONException {
		this(jsonToHistoryCommands(o.getJSONArray("cmds")), jsonToHistoryTags(o.getJSONArray("tags")));
	}
	
	public List<HistoryCommand> getCommands() {
		return mCommands;
	}
	
	public List<HistoryTag> getTags() {
		return mTags;
	}
	
	private static List<HistoryCommand> jsonToHistoryCommands(JSONArray a) {
		ImmutableList.Builder<HistoryCommand> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			try {
				builder.add(new HistoryCommand(a.getJSONObject(i)));
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
		return builder.build();
	}
	
	private static List<HistoryTag> jsonToHistoryTags(JSONArray a) {
		ImmutableList.Builder<HistoryTag> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			try {
				builder.add(new HistoryTag(a.getJSONObject(i)));
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
		return builder.build();
	}

	public static Parcelable.Creator<History> CREATOR =
			new Parcelable.Creator<History>() {
		@SuppressWarnings("unchecked")
		@Override
		public History createFromParcel(Parcel source) {
			List<HistoryCommand> commands = (List<HistoryCommand>) source.readSerializable();
			List<HistoryTag> tags = (List<HistoryTag>) source.readSerializable();
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
		dest.writeSerializable((Serializable) mCommands);
		dest.writeSerializable((Serializable) mTags);
	}
}
