package edu.kufpg.armatus.data;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableList;

import edu.kufpg.armatus.util.ParcelUtils;

public class CommandInfo implements Comparable<CommandInfo>, Parcelable {
	private static final String HELP = "help", NAME = "name", TAGS = "tags", ARG_TYS = "argTys", RES_TY = "resTy";

	private final String mHelp, mName, mResultType;
	private final List<String> mTags, mArgTypes;


	public CommandInfo(String help, String name, List<String> tags, List<String> argTypes, String resultType) {
		this(help, name, ImmutableList.copyOf(tags), ImmutableList.copyOf(argTypes), resultType);
	}

	public CommandInfo(JSONObject o) throws JSONException {
		this(o.getString(HELP), o.getString(NAME), jsonToList(o.getJSONArray(TAGS)),
				jsonToList(o.getJSONArray(ARG_TYS)), o.getString(RES_TY));
	}

	private CommandInfo(String help, String name, ImmutableList<String> tags, ImmutableList<String> argTypes, String resultType) {
		mHelp = help;
		mName = name;
		mTags = tags;
		mArgTypes = argTypes;
		mResultType = resultType;
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

	public List<String> getArgTypes() {
		return mArgTypes;
	}

	public String getResultType() {
		return mResultType;
	}

	private static ImmutableList<String> jsonToList(JSONArray a) throws JSONException {
		ImmutableList.Builder<String> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			builder.add(a.getString(i));
		}
		return builder.build();
	}

	@Override
	public int compareTo(CommandInfo another) {
		int nameComp = getName().compareTo(another.getName());
		if (nameComp == 0) {
			return Integer.valueOf(getArgTypes().size()).compareTo(another.getArgTypes().size());
		} else {
			return nameComp;
		}
	}

	public static Parcelable.Creator<CommandInfo> CREATOR =
			new Parcelable.Creator<CommandInfo>() {
		@Override
		public CommandInfo createFromParcel(Parcel source) {
			String help = source.readString();
			String name = source.readString();
			ImmutableList<String> tags = ParcelUtils.readImmutableList(source, String.class.getClassLoader());
			ImmutableList<String> argTypes = ParcelUtils.readImmutableList(source, String.class.getClassLoader());
			String resultType = source.readString();
			return new CommandInfo(help, name, tags, argTypes, resultType);
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
		dest.writeStringList(mTags);
		dest.writeStringList(mArgTypes);
		dest.writeString(mResultType);
	}

}