package edu.kufpg.armatus.networking.data;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableList;

import edu.kufpg.armatus.util.ParcelUtils;

public class CommandInfo implements Parcelable {
	private final String mHelp, mName;
	private final List<String> mTags;

	public CommandInfo(String help, String name, List<String> tags) {
		this(help, name, ImmutableList.copyOf(tags));
	}

	public CommandInfo(JSONObject o) throws JSONException {
		this(o.getString("help"), o.getString("name"), jsonToTags(o.getJSONArray("tags")));
	}

	private CommandInfo(String help, String name, ImmutableList<String> tags) {
		mHelp = help;
		mName = name;
		mTags = tags;
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

	private static ImmutableList<String> jsonToTags(JSONArray a) {
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
			ImmutableList<String> tags = ParcelUtils.readImmutableList
					(source, CommandInfo.class.getClassLoader());
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
		dest.writeList(mTags);
	}

}