package edu.kufpg.armatus.networking.data;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.collect.ImmutableList;

import edu.kufpg.armatus.util.ParcelUtils;

public class Glyph implements Parcelable {
	private static final String BLUE = "#0090D3";
	private static final String RED = "#CC060B";
	private static final String YELLOW = "#FDFD0D";
	private static final String GREEN = "#1DDA1C";
	private static final String CYAN = "#1BE0CC";
	
	private final GlyphStyle mStyle;
	private final List<Crumb> mPath;
	private final String mText;

	public Glyph(GlyphStyle style, List<Crumb> path, String text) {
		this(style, ImmutableList.copyOf(path), text);
	}

	public Glyph(JSONObject o) throws JSONException {
		this(jsonToStyle(o), jsonToCrumbs(o.getJSONArray("path")), o.getString("text"));
	}
	
	private Glyph(GlyphStyle style, ImmutableList<Crumb> path, String text) {
		mStyle = style;
		mPath = path;
		mText = text;
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
	
	public String getColor() {
		switch (mStyle) {
		case KEYWORD:
			return BLUE;
		case SYNTAX:
			return RED;
		case COERCION:
			return YELLOW;
		case TYPE:
			return GREEN;
		case LIT:
			return CYAN;
		default:
			return null;
		}
	}

	private static ImmutableList<Crumb> jsonToCrumbs(JSONArray a) {
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
		NORMAL, KEYWORD, SYNTAX, VAR, COERCION, TYPE, LIT, WARNING;
	}
	
	public static Parcelable.Creator<Glyph> CREATOR =
			new Parcelable.Creator<Glyph>() {
		@Override
		public Glyph createFromParcel(Parcel source) {
			GlyphStyle style = GlyphStyle.values()[source.readInt()];
			ImmutableList<Crumb> path = ParcelUtils.readImmutableList
					(source, Glyph.class.getClassLoader());
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
		dest.writeInt(mStyle.ordinal());
		dest.writeList(mPath);
		dest.writeString(mText);
	}
}