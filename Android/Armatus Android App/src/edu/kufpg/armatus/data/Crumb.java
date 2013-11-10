package edu.kufpg.armatus.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.base.Optional;

import edu.kufpg.armatus.util.ParcelUtils;

public class Crumb implements Parcelable {
	private static final String CRUMB = "crumb", NUM = "num";

	private final Optional<? extends Integer> mNum;
	private final String mCrumbName;

	public Crumb(String crumbName) {
		this(Optional.<Integer>absent(), crumbName);
	}

	public Crumb(Integer num, String crumbName) {
		this(Optional.fromNullable(num), crumbName);
	}

	public Crumb(JSONObject o) throws JSONException {
		this(jsonToNum(o), o.getString(CRUMB));
	}

	private Crumb(Optional<Integer> num, String crumbName) {
		mNum = num;
		mCrumbName = crumbName;
	}

	public int getNum() throws IllegalStateException {
		return mNum.get();
	}

	public String getCrumbName() {
		return mCrumbName;
	}
	
	public boolean hasNum() {
		return mNum.isPresent();
	}

	private static Optional<Integer> jsonToNum(JSONObject o) throws JSONException {
		if (o.has(NUM)) {
			return Optional.of(o.getInt(NUM));
		} else {
			return Optional.absent();
		}
	}

	@Override
	public boolean equals(Object o) {
		if (!(o instanceof Crumb)) {
			return false;
		}

		Crumb c = (Crumb) o;
		boolean numsEqual;
		if (!hasNum()) {
			numsEqual = !c.hasNum();
		} else {
			numsEqual = getNum() == c.getNum();
		}
		return numsEqual && getCrumbName() == c.getCrumbName();
	}

	public static Parcelable.Creator<Crumb> CREATOR =
			new Parcelable.Creator<Crumb>() {
		@Override
		public Crumb createFromParcel(Parcel source) {
			Optional<Integer> num = ParcelUtils.readOptional
					(source, Integer.class.getClassLoader());
			String crumb = source.readString();
			return new Crumb(num, crumb);
		}

		@Override
		public Crumb[] newArray(int size) {
			return new Crumb[size];
		}
	};

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		ParcelUtils.writeOptional(mNum, dest);
		dest.writeString(mCrumbName);
	}
}