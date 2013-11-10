package edu.kufpg.armatus.data;

import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import android.graphics.Color;
import android.os.Parcel;
import android.os.Parcelable;
import android.text.Spannable;
import android.text.SpannableString;
import android.text.SpannableStringBuilder;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;

import com.google.common.base.Optional;
import com.google.common.collect.ImmutableList;

import edu.kufpg.armatus.data.Glyph.GlyphStyle;
import edu.kufpg.armatus.util.ParcelUtils;
import edu.kufpg.armatus.util.StringUtils;

public class CommandResponse implements Parcelable {
	private static final String AST = "ast", GLYPHS = "glyphs", MSG = "msg";

	private final int mAst;
	private final Optional<? extends List<Glyph>> mGlyphs;
	private final Optional<? extends Spannable> mGlyphText;
	private final Optional<? extends String> mMessage;

	public CommandResponse(int ast) {
		this(ast, Optional.<ImmutableList<Glyph>>absent(), Optional.<String>absent());
	}

	public CommandResponse(int ast, List<Glyph> glyphs) {
		this(ast, Optional.fromNullable(ImmutableList.copyOf(glyphs)), Optional.<String>absent());
	}

	public CommandResponse(int ast, String message) {
		this(ast, Optional.<ImmutableList<Glyph>>absent(), Optional.fromNullable(message));
	}

	public CommandResponse(int ast, List<Glyph> glyphs, String message) {
		this(ast, Optional.fromNullable(ImmutableList.copyOf(glyphs)), Optional.fromNullable(message));
	}

	public CommandResponse(JSONObject o) throws JSONException {
		this(o.getInt(AST), jsonToGlyphs(o), jsonToMessage(o));
	}

	private CommandResponse(int ast, Optional<ImmutableList<Glyph>> glyphs, Optional<String> message) {
		mAst = ast;
		mGlyphs = glyphs;
		mGlyphText = createPrettyText(glyphs);
		mMessage = message;
	}

	private CommandResponse(int ast, Optional<ImmutableList<Glyph>> glyphs,
			Optional<SpannableStringBuilder> glyphText, Optional<String> message) {
		mAst = ast;
		mGlyphs = glyphs;
		mGlyphText = glyphText;
		mMessage = message;
	}

	public int getAst() {
		return mAst;
	}

	public List<Glyph> getGlyphs() throws IllegalStateException {
		return mGlyphs.get();
	}

	public Spannable getGlyphText() throws IllegalStateException {
		return mGlyphText.get();
	}

	public String getMessage() throws IllegalStateException {
		return mMessage.get();
	}

	public boolean hasGlyphs() {
		return mGlyphs.isPresent();
	}

	public boolean hasMessage() {
		return mMessage.isPresent();
	}

	private static Optional<ImmutableList<Glyph>> jsonToGlyphs(JSONObject o) throws JSONException {
		if (o.has(GLYPHS)) {
			ImmutableList.Builder<Glyph> builder = ImmutableList.builder();
			JSONArray a = o.getJSONArray(GLYPHS);
			for (int i = 0; i < a.length(); i++) {
				builder.add(new Glyph(a.getJSONObject(i)));
			}
			return Optional.of(builder.build());
		} else {
			return Optional.absent();
		}
	}

	private static Optional<SpannableStringBuilder> createPrettyText(Optional<? extends List<Glyph>> glyphs) {
		if (glyphs.isPresent()) {
			SpannableStringBuilder builder = new SpannableStringBuilder();
			for (Glyph glyph : glyphs.get()) {
				SpannableString spanWord = new SpannableString(glyph.getText());
				if (glyph.getStyle().equals(GlyphStyle.WARNING)) {
					spanWord.setSpan(new BackgroundColorSpan(Color.YELLOW),
							0, glyph.getText().length(), 0);
					spanWord.setSpan(new ForegroundColorSpan(Color.BLACK),
							0, glyph.getText().length(), 0);
				} else {
					String glyphColor = glyph.getColor();
					if (glyphColor != null) {
						spanWord.setSpan(new ForegroundColorSpan(Color.parseColor(glyphColor)),
								0, glyph.getText().length(), 0);
					}

				}
				builder.append(spanWord);
			}
			return Optional.of(builder);
		} else {
			return Optional.absent();
		}
	}

	private static Optional<String> jsonToMessage(JSONObject o) throws JSONException {
		if (o.has(MSG)) {
			return Optional.of(StringUtils.tightenSpacing(o.getString("msg")));
		} else {
			return Optional.absent();
		}
	}

	public static Parcelable.Creator<CommandResponse> CREATOR =
			new Parcelable.Creator<CommandResponse>() {
		@Override
		public CommandResponse createFromParcel(Parcel source) {
			int ast = source.readInt();
			Optional<ImmutableList<Glyph>> glyphs = ParcelUtils.readOptional
					(source, ImmutableList.class.getClassLoader());
			Optional<SpannableStringBuilder> glyphText = ParcelUtils.readOptional
					(source, SpannableStringBuilder.class.getClassLoader());
			Optional<String> message = ParcelUtils.readOptional(source, String.class.getClassLoader());
			return new CommandResponse(ast, glyphs, glyphText, message);
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
		ParcelUtils.writeOptional(mGlyphs, dest);
		ParcelUtils.writeOptional(mGlyphText, dest);
		ParcelUtils.writeOptional(mMessage, dest);
	}
}