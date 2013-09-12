package edu.kufpg.armatus.networking.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

public class HistoryTag implements Parcelable {
	private final String mTag;
	private final int mAst;
	
	public HistoryTag(String tag, int ast) {
		mTag = tag;
		mAst = ast;
	}
	
	public HistoryTag(JSONObject o) throws JSONException {
		this(o.getString("tag"), o.getInt("ast"));
	}
	
	public String getTag() {
		return mTag;
	}
	
	public int getAst() {
		return mAst;
	}
	
	public static Parcelable.Creator<HistoryTag> CREATOR =
			new Parcelable.Creator<HistoryTag>() {
		@Override
		public HistoryTag createFromParcel(Parcel source) {
			String tag = source.readString();
			int ast = source.readInt();
			return new HistoryTag(tag, ast);
		}

		@Override
		public HistoryTag[] newArray(int size) {
			return new HistoryTag[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeString(mTag);
		dest.writeInt(mAst);
	}

}
