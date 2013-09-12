package edu.kufpg.armatus.networking.data;

import java.io.Serializable;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.graphics.Color;
import android.os.Parcel;
import android.os.Parcelable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;

import com.google.common.collect.ImmutableList;

import edu.kufpg.armatus.networking.data.Glyph.GlyphStyle;

public class CommandResponse implements Parcelable {
	private static final String BLUE = "#0090D3";
	private static final String RED = "#CC060B";
	private static final String YELLOW = "#FDFD0D";
	private static final String GREEN = "#1DDA1C";
	private static final String CYAN = "#1BE0CC";

	private final int mAst;
	private final List<Glyph> mGlyphs;
	private final String mMessage;

	public CommandResponse(int ast, List<Glyph> glyphs, String message) {
		mAst = ast;
		mGlyphs = glyphs;
		mMessage = message;
	}
	
	public CommandResponse(JSONObject o) throws JSONException {
		this(o.getInt("ast"), jsonToGlyphs(o.getJSONArray("glyphs")), o.getString("msg"));
	}

	public CharSequence createPrettyText() {
		SpannableStringBuilder builder = new SpannableStringBuilder();
		for (Glyph glyph : mGlyphs) {
			SpannableString spanWord = new SpannableString(glyph.getText());
			if (glyph.getStyle().equals(GlyphStyle.WARNING)) {
				spanWord.setSpan(new BackgroundColorSpan(Color.YELLOW),
						0, glyph.getText().length(), 0);
				spanWord.setSpan(new ForegroundColorSpan(Color.BLACK),
						0, glyph.getText().length(), 0);
			} else {
				String glyphColor = getGlyphColor(glyph.getStyle());
				if (glyphColor != null) {
					spanWord.setSpan(new ForegroundColorSpan(Color.parseColor(glyphColor)),
							0, glyph.getText().length(), 0);
				}

			}
			builder.append(spanWord);
		}
		return builder;
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

	private static String getGlyphColor(GlyphStyle style) {
		switch (style) {
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

	private static List<Glyph> jsonToGlyphs(JSONArray a) {
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
			@SuppressWarnings("unchecked")
			List<Glyph> glyphs = (List<Glyph>) source.readSerializable();
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
		dest.writeSerializable((Serializable) mGlyphs);
		dest.writeString(mMessage);
	}
}