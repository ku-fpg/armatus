package edu.kufpg.armatus.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

public class Command implements Parcelable {
	private static final String TOKEN = "token", CMD = "cmd";
	
	private final Token mToken;
	private final String mCommand;
	
	public Command(Token token, String command) {
		mToken = token;
		mCommand = command;
	}
	
	public JSONObject toJSONObject() {
		JSONObject o = new JSONObject();
		try {
			o.put(TOKEN, mToken.toJSONObject());
			o.put(CMD, mCommand);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		return o;
	}
	
	@Override
	public String toString() {
		return toJSONObject().toString();
	}
	
	public static Parcelable.Creator<Command> CREATOR =
			new Parcelable.Creator<Command>() {
		@Override
		public Command createFromParcel(Parcel source) {
			Token token = source.readParcelable(Token.class.getClassLoader());
			String command = source.readString();
			return new Command(token, command);
		}

		@Override
		public Command[] newArray(int size) {
			return new Command[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeParcelable(mToken, flags);
		dest.writeString(mCommand);
	}

}
