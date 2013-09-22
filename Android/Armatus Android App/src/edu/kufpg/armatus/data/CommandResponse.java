package edu.kufpg.armatus.data;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableList;

import edu.kufpg.armatus.util.ParcelUtils;
import edu.kufpg.armatus.util.StringUtils;

public class CommandResponse implements Parcelable {
	private final int mAst;
	private final List<Glyph> mGlyphs;
	private final String mMessage;

	public CommandResponse(int ast, List<Glyph> glyphs, String message) {
		this(ast, ImmutableList.copyOf(glyphs), message);
	}
	
	public CommandResponse(JSONObject o) throws JSONException {
		this(o.getInt("ast"), (o.has("glyphs") ? jsonToGlyphs(o.getJSONArray("glyphs")) : null),
				(o.has("msg") ? StringUtils.tightenSpacing(o.getString("msg")) : null));
	}
	
	private CommandResponse(int ast, ImmutableList<Glyph> glyphs, String message) {
		mAst = ast;
		mGlyphs = glyphs;
		mMessage = message;
	}

	public int getAst() {
		return mAst;
	}

	public List<Glyph> getGlyphs() {
		return mGlyphs;
	}
	
	public String getMessage() {
		return mMessage;
	}

	private static ImmutableList<Glyph> jsonToGlyphs(JSONArray a) {
		ImmutableList.Builder<Glyph> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			try {
				builder.add(new Glyph(a.getJSONObject(i)));
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
		return builder.build();
	}
	
	public static Parcelable.Creator<CommandResponse> CREATOR =
			new Parcelable.Creator<CommandResponse>() {
		@Override
		public CommandResponse createFromParcel(Parcel source) {
			int ast = source.readInt();
			ImmutableList<Glyph> glyphs = ParcelUtils.readImmutableList
					(source, CommandResponse.class.getClassLoader());
			String message = source.readString();
			return new CommandResponse(ast, glyphs, message);
		}

		@Override
		public CommandResponse[] newArray(int size) {
			return new CommandResponse[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(mAst);
		dest.writeList(mGlyphs);
		dest.writeString(mMessage);
	}
}