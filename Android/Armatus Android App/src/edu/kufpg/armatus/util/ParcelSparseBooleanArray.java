package edu.kufpg.armatus.util;

import android.os.Parcel;
import android.os.Parcelable;
import android.util.SparseBooleanArray;

/**
 * Extension of {@link SparseBooleanArray} that is {@link Parcelable}.
 * Credit goes to <a href="http://stackoverflow.com/a/16711258/1567086">opsidao of Stack Overflow</a>.
 */
public class ParcelSparseBooleanArray extends SparseBooleanArray implements Parcelable {
	public static Parcelable.Creator<ParcelSparseBooleanArray> CREATOR =
			new Parcelable.Creator<ParcelSparseBooleanArray>() {
		@Override
		public ParcelSparseBooleanArray createFromParcel(Parcel source) {
			ParcelSparseBooleanArray read = new ParcelSparseBooleanArray();
			int size = source.readInt();

			int[] keys = new int[size];
			boolean[] values = new boolean[size];

			source.readIntArray(keys);
			source.readBooleanArray(values);

			for (int i = 0; i < size; i++) {
				read.put(keys[i], values[i]);
			}

			return read;
		}

		@Override
		public ParcelSparseBooleanArray[] newArray(int size) {
			return new ParcelSparseBooleanArray[size];
		}
	};

	/** Constructs a new {@link ParcelSparseBooleanArray} with no mappings. */
	public ParcelSparseBooleanArray() {}

	/**
	 * Constructs a new {@link ParcelSparseBooleanArray} instance containing the
	 * mappings of the specified {@link SparseBooleanArray}.
	 * @param sparseBooleanArray The array to copy.
	 */
	public ParcelSparseBooleanArray(SparseBooleanArray sparseBooleanArray) {
		for (int i = 0; i < sparseBooleanArray.size(); i++) {
			put(sparseBooleanArray.keyAt(0), sparseBooleanArray.valueAt(0));
		}
	}

	@Override
	public int describeContents() {
		return 0;
	}

	@Override
	public void writeToParcel(Parcel dest, int flags) {
		int[] keys = new int[size()];
		boolean[] values = new boolean[size()];

		for (int i = 0; i < size(); i++) {
			keys[i] = keyAt(i);
			values[i] = valueAt(i);
		}

		dest.writeInt(size());
		dest.writeIntArray(keys);
		dest.writeBooleanArray(values);
	}
}