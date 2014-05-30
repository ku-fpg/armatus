package edu.kufpg.armatus.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.base.Optional;

import edu.kufpg.armatus.util.ParcelUtils;

public class Command implements Parcelable {
    private static final String TOKEN = "token", CMD = "cmd", WIDTH = "width";

    private final Token mToken;
    private final String mCommand;
    private final Optional<Integer> mWidth;

    public Command(final Token token, final String command) {
        this(token, command, Optional.<Integer>absent());
    }

    public Command(final Token token, final String command, final int width) {
        this(token, command, Optional.of(width));
    }

    private Command(final Token token, final String command, final Optional<Integer> width) {
        mToken = token;
        mCommand = command;
        mWidth = width;
    }

    public int getWidth() {
        return mWidth.get();
    }

    public boolean hasWidth() {
        return mWidth.isPresent();
    }

    public JSONObject toJSONObject() {
        final JSONObject o = new JSONObject();
        try {
            o.put(TOKEN, mToken.toJSONObject());
            o.put(CMD, mCommand);
            if (hasWidth()) {
                o.put(WIDTH, getWidth());
            }
        } catch (final JSONException e) {
            e.printStackTrace();
        }
        return o;
    }

    @Override
    public String toString() {
        return toJSONObject().toString();
    }

    public static final Parcelable.Creator<Command> CREATOR =
            new Parcelable.Creator<Command>() {
                @Override
                public Command createFromParcel(final Parcel source) {
                    final Token token = source.readParcelable(Token.class.getClassLoader());
                    final String command = source.readString();
                    final Optional<Integer> width = ParcelUtils.readOptional(source);
                    return new Command(token, command, width);
                }

                @Override
                public Command[] newArray(final int size) {
                    return new Command[size];
                }
            };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeParcelable(mToken, flags);
        dest.writeString(mCommand);
        ParcelUtils.writeOptional(dest, mWidth);
    }

}
