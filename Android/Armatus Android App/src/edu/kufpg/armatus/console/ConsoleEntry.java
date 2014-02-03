package edu.kufpg.armatus.console;

import java.util.ArrayList;

import android.graphics.Color;
import android.os.Parcel;
import android.os.Parcelable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;

import com.google.common.base.Objects;

import edu.kufpg.armatus.data.CommandResponse;
import edu.kufpg.armatus.util.ParcelUtils;
import edu.kufpg.armatus.util.StringUtils;

/**
 * Contains values that describe an entry in {@link ConsoleListView1}. This class
 * is primarily used for populating {@link android.view.View Views} in {@link
 * ConsoleEntryAdapter1}.
 */
public class ConsoleEntry implements Parcelable {
	/** 
	 * The unique entry number used to identify this entry. Entry numbers begin
	 * at 0 (although the first entry in the console may not be 0 since the number
	 * of entries could exceed the {@link ConsoleActivity1#CONSOLE_ENTRY_LIMIT
	 * CONSOLE_ENTRY_LIMIT}).
	 */
	private int mEntryNum;
	private int mAst;

	private String mUserInput;
	private CommandResponse mCommandResponse;
	private String mErrorResponse;
	private CharSequence mShortContents;
	private ArrayList<CharSequence> mContentLines;

	public ConsoleEntry(int entryNum, int ast, String userInput) {
		this(entryNum, ast, userInput, null, null);
	}

	public ConsoleEntry(int entryNum, int ast, String userInput, CommandResponse commandResponse) {
		this(entryNum, ast, userInput, commandResponse, null);
	}

	public ConsoleEntry(int entryNum, int ast, String userInput, String errorResponse) {
		this(entryNum, ast, userInput, null, errorResponse);
	}

	protected ConsoleEntry(int entryNum, int ast, String userInput, CommandResponse commandResponse, String errorResponse) {
		this(entryNum, ast, userInput, commandResponse, errorResponse, buildShortContents(userInput, commandResponse, errorResponse));
	}

	protected ConsoleEntry(int entryNum, int ast, String userInput, CommandResponse commandResponse,
			String errorResponse, CharSequence shortContents) {
		mEntryNum = entryNum;
		mAst = ast;
		mUserInput = userInput;
		mCommandResponse = commandResponse;
		mErrorResponse = errorResponse;
		mShortContents = shortContents;
		mContentLines = makeContentLines(getFullContents());
	}
	
	protected ConsoleEntry(int entryNum, int ast, String userInput, CommandResponse commandResponse,
			String errorResponse, CharSequence shortContents, ArrayList<CharSequence> contentLines) {
		mEntryNum = entryNum;
		mAst = ast;
		mUserInput = userInput;
		mCommandResponse = commandResponse;
		mErrorResponse = errorResponse;
		mShortContents = shortContents;
		mContentLines = contentLines;
	}

	/**
	 * Constructs a new instance with the specified {@link ConsoleEntry}'s number
	 * and contents.
	 * @param entry the {@code ConsoleEntry} to copy.
	 */
	public ConsoleEntry(ConsoleEntry entry) {
		this(entry.getEntryNum(), entry.getAst(), entry.getUserInput(), entry.getCommandResponse(),
				entry.getErrorResponse(), entry.getShortContents(), entry.getContentLines());
	}

	public int getAst() {
		return mAst;
	}

	/**
	 * Returns the entry's unique number.
	 * @return the number used to identify this entry.
	 */
	public int getEntryNum() {
		return mEntryNum;
	}

	public String getUserInput() {
		return mUserInput;
	}

	public CommandResponse getCommandResponse() {
		return mCommandResponse;
	}

	public String getErrorResponse() {
		return mErrorResponse;
	}

	/**
	 * Returns this entry's contents without the {@code hermit<ast> }prefix.
	 * @return the unadorned entry contents.
	 */
	public CharSequence getShortContents() {
		return mShortContents;
	}
	
	public ArrayList<CharSequence> getContentLines() {
		return mContentLines;
	}

	/**
	 * Returns this entry's contents including the {@code hermit<ast> }prefix.
	 * @return the entry contents, including the prefix.
	 */
	public CharSequence getFullContents() {
		return getFullContentsPrefix().append(getShortContents());
	}

	public SpannableStringBuilder getFullContentsPrefix() {
		SpannableStringBuilder prefix = new SpannableStringBuilder(StringUtils.NBSP);
		if (mAst != HermitClient.NO_TOKEN) {
			prefix = prefix.append("hermit<" + mAst + ">");
		} else {
			prefix = prefix.append("armatus");
		}
		prefix = prefix.append(StringUtils.NBSP);
		prefix.setSpan(new BackgroundColorSpan(Color.DKGRAY), 0, prefix.length(), 0);
		prefix.setSpan(new ForegroundColorSpan(Color.WHITE), 0, prefix.length(), 0);
		return prefix.append(StringUtils.NBSP);
	}

	public void appendCommandResponse(CommandResponse commandResponse) {
		mCommandResponse = commandResponse;
		mShortContents = buildShortContents(mUserInput, mCommandResponse, mErrorResponse);
		mContentLines = makeContentLines(getFullContents());
	}

	public void appendErrorResponse(String errorResponse) {
		mErrorResponse = errorResponse;
		mShortContents = buildShortContents(mUserInput, mCommandResponse, mErrorResponse);
		mContentLines = makeContentLines(getFullContents());
	}

	void setUserInput(String userInput) {
		mUserInput = userInput;
		mShortContents = buildShortContents(mUserInput, mCommandResponse, mErrorResponse);
		mContentLines = makeContentLines(getFullContents());
	}
	
	private static ArrayList<CharSequence> makeContentLines(CharSequence contents) {
		ArrayList<CharSequence> contentLines = new ArrayList<CharSequence>();
		int lineStart = 0;
		int length = contents.length();
		
		for (int i = 0; i < length; i++) {
			if (contents.charAt(i) == '\n') {
				contentLines.add(contents.subSequence(lineStart, i));
				lineStart = i + 1;
			}
		}
		contentLines.add(contents.subSequence(lineStart, length));
		return contentLines;
	}

	private static CharSequence buildShortContents(String userInput, CommandResponse commandResponse, String errorResponse) {
		SpannableStringBuilder builder = new SpannableStringBuilder();
		if (userInput != null) {
			builder.append(userInput).append("\n");
		}
		if (commandResponse != null) {
			if (commandResponse.hasGlyphs()) {
				builder.append(commandResponse.getGlyphText()).append("\n");
			}
			if (commandResponse.hasMessage()) {
				builder.append(commandResponse.getMessage()).append("\n");
			}
		}
		if (errorResponse != null) {
			builder.append(errorResponse).append("\n");
		}

		if (builder.length() > 0) {
			builder = builder.delete(builder.length()-1, builder.length());
		}
		return builder;
	}
	
	@Override
	public boolean equals(Object o) {
		if (o instanceof ConsoleEntry) {
			ConsoleEntry ce = (ConsoleEntry) o;
			return mAst == ce.getAst()
					&& mEntryNum == ce.getEntryNum()
					&& mUserInput.equals(ce.getUserInput())
					&& mErrorResponse.equals(ce.getErrorResponse())
					&& mCommandResponse.equals(ce.getCommandResponse());
		} else {
			return false;
		}
	}
	
	@Override
	public int hashCode() {
		return Objects.hashCode(mAst, mEntryNum, mUserInput, mCommandResponse, mErrorResponse);
	}

	public static final Parcelable.Creator<ConsoleEntry> CREATOR
	= new Parcelable.Creator<ConsoleEntry>() {
		@Override
		public ConsoleEntry createFromParcel(Parcel in) {
			int entryNum = in.readInt();
			int ast = in.readInt();
			String userInput = in.readString();
			CommandResponse commandResponse = in.readParcelable(CommandResponse.class.getClassLoader());
			String errorResponse = in.readString();
			CharSequence shortContents = TextUtils.CHAR_SEQUENCE_CREATOR.createFromParcel(in);
			ArrayList<CharSequence> contentLines = ParcelUtils.readArrayList(in);
			return new ConsoleEntry(entryNum, ast, userInput, commandResponse, errorResponse, shortContents, contentLines);
		}

		@Override
		public ConsoleEntry[] newArray(int size) {
			return new ConsoleEntry[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(mEntryNum);
		dest.writeInt(mAst);
		dest.writeString(mUserInput);
		dest.writeParcelable(mCommandResponse, flags);
		dest.writeString(mErrorResponse);
		TextUtils.writeToParcel(mShortContents, dest, flags);
		ParcelUtils.writeCollection(dest, mContentLines);
	}

}