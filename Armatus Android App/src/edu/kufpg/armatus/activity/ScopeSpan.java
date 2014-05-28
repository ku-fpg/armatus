package edu.kufpg.armatus.activity;

import com.google.common.base.Objects;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.style.ClickableSpan;
import android.view.View;

public class ScopeSpan extends ClickableSpan implements Parcelable {

	private final int mStartIndex, mEndIndex;

	public ScopeSpan(int startIndex, int endIndex) {
		super();
		mStartIndex = startIndex;
		mEndIndex = endIndex;
	}

	public int getEndIndex() {
		return mEndIndex;
	}

	public int getStartIndex() {
		return mStartIndex;
	}

	@Override
	public boolean equals(Object o) {
		if (o instanceof ScopeSpan) {
			ScopeSpan other = (ScopeSpan) o;
			return mStartIndex == other.getStartIndex() &&
					mEndIndex == other.getEndIndex();
		} else {
			return false;
		}
	}
	
	@Override
	public int hashCode() {
		return Objects.hashCode(mStartIndex, mEndIndex);
	}

	@Override
	public void onClick(View widget) {}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		dest.writeInt(mStartIndex);
		dest.writeInt(mEndIndex);
	}

	public static Parcelable.Creator<ScopeSpan> CREATOR =
			new Parcelable.Creator<ScopeSpan>() {
		@Override
		public ScopeSpan createFromParcel(Parcel source) {
			int startIndex = source.readInt();
			int endIndex = source.readInt();
			return new ScopeSpan(startIndex, endIndex);
		}

		@Override
		public ScopeSpan[] newArray(int size) {
			return new ScopeSpan[size];
		}
	};

}
