package edu.kufpg.armatus.console;

import com.google.common.collect.ComparisonChain;

import android.os.Parcel;
import android.os.Parcelable;

public class ConsoleLineParams implements Comparable<ConsoleLineParams>, Parcelable {
	public final int entryNum;
	public final int lineNum;

	public ConsoleLineParams(int entryNum, int lineNum) {
		this.entryNum = entryNum;
		this.lineNum = lineNum;
	}
	
	@Override
	public int compareTo(ConsoleLineParams another) {
		return ComparisonChain.start()
				.compare(entryNum, another.entryNum)
				.compare(lineNum, another.lineNum)
				.result();
	}


	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(entryNum);
		dest.writeInt(lineNum);
	}

	public static final Parcelable.Creator<ConsoleLineParams> CREATOR
	= new Parcelable.Creator<ConsoleLineParams>() {

		@Override
		public ConsoleLineParams createFromParcel(Parcel source) {
			int entryNum = source.readInt();
			int lineNum = source.readInt();
			return new ConsoleLineParams(entryNum, lineNum);
		}

		@Override
		public ConsoleLineParams[] newArray(int size) {
			return new ConsoleLineParams[size];
		}

	};
}
