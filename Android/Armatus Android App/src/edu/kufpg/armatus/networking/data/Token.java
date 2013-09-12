package edu.kufpg.armatus.networking.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

public class Token implements Parcelable {
	private final int mUser;
	private int mAst;

	public Token(int user, int ast) {
		mUser = user;
		mAst = ast;
	}
	
	public Token(JSONObject o) throws JSONException {
		this(o.getInt("user"), o.getInt("ast"));
	}

	public int getUser() {
		return mUser;
	}

	public int getAst() {
		return mAst;
	}

	public void setAst(int ast) {
		mAst = ast;
	}

	public JSONObject toJSONObject() {
		JSONObject o = new JSONObject();
		try {
			o.put("user", mUser);
			o.put("ast", mAst);
		} catch (JSONException e) {
			e.printStackTrace();
		}
		return o;
	}
	
	public static Parcelable.Creator<Token> CREATOR =
			new Parcelable.Creator<Token>() {
		@Override
		public Token createFromParcel(Parcel source) {
			int user = source.readInt();
			int ast = source.readInt();
			return new Token(user, ast);
		}

		@Override
		public Token[] newArray(int size) {
			return new Token[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(mUser);
		dest.writeInt(mAst);
	}
}