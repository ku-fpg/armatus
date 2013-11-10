package edu.kufpg.armatus.console;

import android.graphics.Color;
import android.os.Parcel;
import android.os.Parcelable;
import android.text.SpannableStringBuilder;
import android.text.TextUtils;
import android.text.style.BackgroundColorSpan;
import android.text.style.ForegroundColorSpan;

import edu.kufpg.armatus.data.CommandResponse;
import edu.kufpg.armatus.util.StringUtils;

/**
 * Contains values that describe an entry in {@link ConsoleListView}. This class
 * is primarily used for populating {@link android.view.View Views} in {@link
 * ConsoleEntryAdapter}.
 */
public class ConsoleEntry implements Parcelable {
	/** 
	 * The unique entry number used to identify this entry. Entry numbers begin
	 * at 0 (although the first entry in the console may not be 0 since the number
	 * of entries could exceed the {@link ConsoleActivity#CONSOLE_ENTRY_LIMIT
	 * CONSOLE_ENTRY_LIMIT}).
	 */
	private int mEntryNum;
	private int mAst;

	private String mUserInput;
	private CommandResponse mCommandResponse;
	private String mErrorResponse;
	private CharSequence mShortContents;

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
		this(entryNum, ast, userInput, commandResponse, errorResponse, null);
		mShortContents = buildShortContents(userInput, commandResponse, errorResponse);
	}

	protected ConsoleEntry(int entryNum, int ast, String userInput, CommandResponse commandResponse,
			String errorResponse, CharSequence shortContents) {
		mEntryNum = entryNum;
		mAst = ast;
		mUserInput = userInput;
		mCommandResponse = commandResponse;
		mErrorResponse = errorResponse;
		mShortContents = shortContents;
	}

	/**
	 * Constructs a new instance with the specified {@link ConsoleEntry}'s number
	 * and contents.
	 * @param entry the {@code ConsoleEntry} to copy.
	 */
	public ConsoleEntry(ConsoleEntry entry) {
		this(entry.getEntryNum(), entry.getAst(), entry.getUserInput(), entry.getCommandResponse(),
				entry.getErrorResponse(), entry.getShortContents());
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
	}

	public void appendErrorResponse(String errorResponse) {
		mErrorResponse = errorResponse;
		mShortContents = buildShortContents(mUserInput, mCommandResponse, mErrorResponse);
	}

	void setUserInput(String userInput) {
		mUserInput = userInput;
		mShortContents = buildShortContents(mUserInput, mCommandResponse, mErrorResponse);
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

	public static final Parcelable.Creator<ConsoleEntry> CREATOR
	= new Parcelable.Creator<ConsoleEntry>() {
		public ConsoleEntry createFromParcel(Parcel in) {
			int entryNum = in.readInt();
			int ast = in.readInt();
			String userInput = in.readString();
			CommandResponse commandResponse = in.readParcelable(CommandResponse.class.getClassLoader());
			String errorResponse = in.readString();
			CharSequence shortContents = TextUtils.CHAR_SEQUENCE_CREATOR.createFromParcel(in);
			return new ConsoleEntry(entryNum, ast, userInput, commandResponse, errorResponse, shortContents);
		}

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
	}

}