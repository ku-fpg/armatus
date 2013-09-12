package edu.kufpg.armatus.networking.data;

import java.io.Serializable;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableList;

public class Glyph implements Parcelable {
	private final GlyphStyle mStyle;
	private final List<Crumb> mPath;
	private final String mText;

	public Glyph(GlyphStyle style, List<Crumb> path, String text) {
		mStyle = style;
		mPath = path;
		mText = text;
	}

	public Glyph(JSONObject o) throws JSONException {
		this(jsonToStyle(o), jsonToCrumbs(o.getJSONArray("path")), o.getString("text"));
	}

	public GlyphStyle getStyle() {
		return mStyle;
	}

	public List<Crumb> getPath() {
		return mPath;
	}

	public String getText() {
		return mText;
	}

	private static List<Crumb> jsonToCrumbs(JSONArray a) {
		ImmutableList.Builder<Crumb> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			try {
				builder.add(new Crumb(a.getJSONObject(i)));
			} catch (JSONException e) {
				e.printStackTrace();
			}
		}
		return builder.build();
	}

	private static GlyphStyle jsonToStyle(JSONObject o) {
		if (o.has("style")) {
			GlyphStyle glyphStyle = null;
			try {
				glyphStyle = GlyphStyle.valueOf(o.getString("style"));
			} catch (JSONException e) {
				e.printStackTrace();
			}

			return glyphStyle;
		} else {
			return GlyphStyle.NORMAL;
		}

	}
	
	public static enum GlyphStyle {
		NORMAL, KEYWORD, SYNTAX, VAR, COERCION, TYPE, LIT, WARNING
	}
	
	public static Parcelable.Creator<Glyph> CREATOR =
			new Parcelable.Creator<Glyph>() {
		@Override
		public Glyph createFromParcel(Parcel source) {
			GlyphStyle style = (GlyphStyle) source.readSerializable();
			@SuppressWarnings("unchecked")
			List<Crumb> path = (List<Crumb>) source.readSerializable();
			String text = source.readString();
			return new Glyph(style, path, text);
		}

		@Override
		public Glyph[] newArray(int size) {
			return new Glyph[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeSerializable(mStyle);
		dest.writeSerializable((Serializable) mPath);
		dest.writeString(mText);
	}
}