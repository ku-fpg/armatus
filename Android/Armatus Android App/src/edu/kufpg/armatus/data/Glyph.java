package edu.kufpg.armatus.data;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.base.Objects;
import com.google.common.base.Optional;
import com.google.common.collect.ImmutableList;

import edu.kufpg.armatus.util.ParcelUtils;

public class Glyph implements Parcelable {
	public static final String BLUE = "#0090D3";
	public static final String RED = "#CC060B";
	public static final String YELLOW = "#FDFD0D";
	public static final String GREEN = "#1DDA1C";
	public static final String CYAN = "#1BE0CC";

	private static final String STYLE = "style", BINDING_SITE = "bindingSite", PATH = "path", TEXT = "text";

	private final GlyphStyle mStyle;
	private final Optional<? extends List<Crumb>> mBindingSite;
	private final List<Crumb> mPath;
	private final String mText;

	public Glyph(GlyphStyle style, List<Crumb> path, String text) {
		this(style, Optional.<ImmutableList<Crumb>>absent(), ImmutableList.copyOf(path), text);
	}

	public Glyph(GlyphStyle style, List<Crumb> bindingSite, List<Crumb> path, String text) {
		this(style, Optional.fromNullable(ImmutableList.copyOf(bindingSite)), ImmutableList.copyOf(path), text);
	}

	public Glyph(JSONObject o) throws JSONException {
		this(jsonToStyle(o), jsonToBindingSite(o, BINDING_SITE), jsonToPath(o.getJSONArray(PATH)), o.getString(TEXT));
	}

	private Glyph(GlyphStyle style, Optional<ImmutableList<Crumb>> bindingSite, ImmutableList<Crumb> path, String text) {
		mStyle = style;
		mBindingSite = bindingSite;
		mPath = path;
		mText = text;
	}

	public GlyphStyle getStyle() {
		return mStyle;
	}

	public List<Crumb> getBindingSite() throws IllegalStateException {
		return mBindingSite.get();
	}

	public List<Crumb> getPath() {
		return mPath;
	}

	public String getText() {
		return mText;
	}

	public boolean hasBindingSite() {
		return mBindingSite.isPresent();
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

	private static Optional<ImmutableList<Crumb>> jsonToBindingSite(JSONObject o, String name) throws JSONException {
		if (o.has(name)) {
			ImmutableList.Builder<Crumb> builder = ImmutableList.builder();
			JSONArray a = o.getJSONArray(name);
			for (int i = 0; i < a.length(); i++) {
				builder.add(new Crumb(a.getJSONObject(i)));
			}
			return Optional.of(builder.build());
		} else {
			return Optional.absent();
		}
	}

	private static ImmutableList<Crumb> jsonToPath(JSONArray a) throws JSONException {
		ImmutableList.Builder<Crumb> builder = ImmutableList.builder();
		for (int i = 0; i < a.length(); i++) {
			builder.add(new Crumb(a.getJSONObject(i)));
		}
		return builder.build();
	}

	private static GlyphStyle jsonToStyle(JSONObject o) throws JSONException {
		if (o.has(STYLE)) {
			GlyphStyle glyphStyle = null;
			glyphStyle = GlyphStyle.valueOf(o.getString("style"));
			return glyphStyle;
		} else {
			return GlyphStyle.NORMAL;
		}

	}

	public static enum GlyphStyle {
		NORMAL, KEYWORD, SYNTAX, VAR, COERCION, TYPE, LIT, WARNING;
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof Glyph) {
			Glyph g = (Glyph) o;
			return mStyle.equals(g.getStyle()) && mBindingSite.equals(g.mBindingSite)
					&& mPath.equals(g.getPath()) && mText.equals(g.getText());
		} else {
			return false;
		}
	}
	
	@Override
	public int hashCode() {
		return Objects.hashCode(mStyle, mBindingSite, mPath, mText);
	}
	
	@Override
	public String toString() {
		return mText;
	}

	public static Parcelable.Creator<Glyph> CREATOR =
			new Parcelable.Creator<Glyph>() {
		@Override
		public Glyph createFromParcel(Parcel source) {
			GlyphStyle style = GlyphStyle.values()[source.readInt()];
			Optional<ImmutableList<Crumb>> bindingSite = ParcelUtils.readOptional(source);
			ImmutableList<Crumb> path = ParcelUtils.readImmutableList(source);
			String text = source.readString();
			return new Glyph(style, bindingSite, path, text);
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
		ParcelUtils.writeOptional(dest, mBindingSite);
		dest.writeTypedList(mPath);
		dest.writeString(mText);
	}
}