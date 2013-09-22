package edu.kufpg.armatus.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

public class Crumb implements Parcelable {
	private final int mNum;
	private final String mCrumb;

	public Crumb(int num, String crumb) {
		mNum = num;
		mCrumb = crumb;
	}

	public Crumb(JSONObject o) throws JSONException {
		this(jsonToNum(o), o.getString("crumb"));
	}

	public int getNum() {
		return mNum;
	}

	public String getCrumb() {
		return mCrumb;
	}

	private static int jsonToNum(JSONObject o) {
		int num = -1;
		if (o.has("num")) {
			try {
				num = o.getInt("style");
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
		return num;
	}
	
	public static Parcelable.Creator<Crumb> CREATOR =
			new Parcelable.Creator<Crumb>() {
		@Override
		public Crumb createFromParcel(Parcel source) {
			int num = source.readInt();
			String crumb = source.readString();
			return new Crumb(num, crumb);
		}

		@Override
		public Crumb[] newArray(int size) {
			return new Crumb[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(mNum);
		dest.writeString(mCrumb);
	}
}