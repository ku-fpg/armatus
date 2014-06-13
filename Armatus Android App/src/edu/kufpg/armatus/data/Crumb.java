package edu.kufpg.armatus.data;

import android.support.annotation.NonNull;
import android.support.annotation.Nullable;
import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.base.Objects;
import com.google.common.base.Optional;

import edu.kufpg.armatus.util.ParcelUtils;

public class Crumb implements Parcelable {
    private static final String CRUMB = "crumb", NUM = "num";

    private final Optional<Integer> mNum;
    private final String mCrumbName;

    public Crumb(@NonNull final String crumbName) {
        this(Optional.<Integer>absent(), crumbName);
    }

    public Crumb(@Nullable final Integer num, @NonNull final String crumbName) {
        this(Optional.fromNullable(num), crumbName);
    }

    public Crumb(@NonNull final JSONObject o) throws JSONException {
        this(jsonToNum(o), o.getString(CRUMB));
    }

    private Crumb(@NonNull final Optional<Integer> num, @NonNull final String crumbName) {
        mNum = num;
        mCrumbName = crumbName;
    }

    @NonNull private static Optional<Integer>jsonToNum(@NonNull final JSONObject o)
            throws JSONException {
        if (o.has(NUM)) {
            return Optional.of(o.getInt(NUM));
        } else {
            return Optional.absent();
        }
    }

    public int getNum() {
        return mNum.get();
    }

    @NonNull public String getCrumbName() {
        return mCrumbName;
    }

    public boolean hasNum() {
        return mNum.isPresent();
    }

    @Override public boolean equals(@Nullable final Object o) {
        if (o instanceof Crumb) {
            final Crumb c = (Crumb) o;
            return mNum.equals(c.mNum) && mCrumbName.equals(c.getCrumbName());
        } else {
            return false;
        }
    }

    @Override public int hashCode() {
        return Objects.hashCode(mNum, mCrumbName);
    }

    @Override public int describeContents() {
        return 0;
    }

    @Override public void writeToParcel(@NonNull final Parcel dest, final int flags) {
        ParcelUtils.writeOptional(dest, mNum);
        dest.writeString(mCrumbName);
    }

    public static final Parcelable.Creator<Crumb> CREATOR =
            new Parcelable.Creator<Crumb>() {
                @NonNull
                @Override
                public Crumb createFromParcel(@NonNull final Parcel source) {
                    final Optional<Integer> num = ParcelUtils.readOptional(source);
                    final String crumb = source.readString();
                    return new Crumb(num, crumb);
                }

                @NonNull
                @Override
                public Crumb[] newArray(final int size) {
                    return new Crumb[size];
                }
            };
}