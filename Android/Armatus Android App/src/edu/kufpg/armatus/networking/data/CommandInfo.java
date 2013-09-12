package edu.kufpg.armatus.networking.data;

import java.io.Serializable;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableList;

public class CommandInfo implements Parcelable {
	private final String mHelp, mName;
	private final List<String> mTags;

	public CommandInfo(String help, String name, List<String> tags) {
		mHelp = help;
		mName = name;
		mTags = tags;
	}
	
	public CommandInfo(JSONObject o) throws JSONException {
		this(o.getString("help"), o.getString("name"), jsonToTags(o.getJSONArray("tags")));
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
	
	private static List<String> jsonToTags(JSONArray a) {
		ImmutableList.Builder<String> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			try {
				builder.add(a.getString(i));
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
		return builder.build();
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