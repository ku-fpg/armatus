package edu.kufpg.armatus.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

public class Complete implements Parcelable {
	private final int mUser;
	private final String mCommand;
	
	public Complete(int user, String command) {
		mUser = user;
		mCommand = command;
	}
	
	public int getUser() {
		return mUser;
	}
	
	public String getCommand() {
		return mCommand;
	}
	
	public JSONObject toJSONObject() {
		JSONObject o = new JSONObject();
		try {
			o.put("user", mUser);
			o.put("cmd", mCommand);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		return o;
	}
	
	@Override
	public String toString() {
		return toJSONObject().toString();
	}

	public static Parcelable.Creator<Complete> CREATOR =
			new Parcelable.Creator<Complete>() {
		@Override
		public Complete createFromParcel(Parcel source) {
			int user = source.readInt();
			String command = source.readString();
			return new Complete(user, command);
		}

		@Override
		public Complete[] newArray(int size) {
			return new Complete[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(mUser);
		dest.writeString(mCommand);
	}
}
