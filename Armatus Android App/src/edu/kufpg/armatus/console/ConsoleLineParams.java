package edu.kufpg.armatus.console;

import android.os.Parcel;
import android.os.Parcelable;
import com.google.common.base.Objects;
import com.google.common.collect.ComparisonChain;

public class ConsoleLineParams implements Comparable<ConsoleLineParams>, Parcelable {
    public final int entryNum;
    public final int lineNum;

    public ConsoleLineParams(final int entryNum, final int lineNum) {
        this.entryNum = entryNum;
        this.lineNum = lineNum;
    }

    @Override public boolean equals(final Object o) {
        if (o instanceof ConsoleLineParams) {
            final ConsoleLineParams clp = (ConsoleLineParams) o;
            return entryNum == clp.entryNum
                    && lineNum == clp.lineNum;
        } else {
            return false;
        }
    }

    @Override public int hashCode() {
        return Objects.hashCode(entryNum, lineNum);
    }

    @Override public int compareTo(final ConsoleLineParams another) {
        return ComparisonChain.start()
                .compare(entryNum, another.entryNum)
                .compare(lineNum, another.lineNum)
                .result();
    }

    @Override public int describeContents() {
        return 0;
    }

    @Override public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeInt(entryNum);
        dest.writeInt(lineNum);
    }

    public static final Parcelable.Creator<ConsoleLineParams> CREATOR
            = new Parcelable.Creator<ConsoleLineParams>() {

        @Override public ConsoleLineParams createFromParcel(final Parcel source) {
            int entryNum = source.readInt();
            int lineNum = source.readInt();
            return new ConsoleLineParams(entryNum, lineNum);
        }

        @Override public ConsoleLineParams[] newArray(final int size) {
            return new ConsoleLineParams[size];
        }

    };
}