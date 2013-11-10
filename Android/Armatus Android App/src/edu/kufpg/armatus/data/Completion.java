package edu.kufpg.armatus.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

public class Completion implements Parcelable {
	private static final String IS_FINISHED = "isFinished", REPLACEMENT = "replacement", DISPLAY = "display";
	
	private final boolean mIsFinished;
	private final String mReplacement;
	private final String mDisplay;
	
	public Completion(boolean isFinished, String replacement, String display) {
		mIsFinished = isFinished;
		mReplacement = replacement;
		mDisplay = display;
	}
	
	public Completion(JSONObject o) throws JSONException {
		this(o.getBoolean(IS_FINISHED), o.getString(REPLACEMENT), o.getString(DISPLAY));
	}
	
	public boolean isFinished() {
		return mIsFinished;
	}
	
	public String getReplacement() {
		return mReplacement;
	}
	
	public String getDisplay() {
		return mDisplay;
	}
	
	public static Parcelable.Creator<Completion> CREATOR =
			new Parcelable.Creator<Completion>() {
		@Override
		public Completion createFromParcel(Parcel source) {
			boolean isFinished = (source.readInt() == 1) ? true : false;
			String replacement = source.readString();
			String display = source.readString();
			return new Completion(isFinished, replacement, display);
		}

		@Override
		public Completion[] newArray(int size) {
			return new Completion[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(mIsFinished ? 1 : 0);
		dest.writeString(mReplacement);
		dest.writeString(mDisplay);
	}

}
