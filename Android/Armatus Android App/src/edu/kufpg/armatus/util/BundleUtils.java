package edu.kufpg.armatus.util;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import android.os.Bundle;
import android.os.Parcelable;

import com.google.common.collect.BoundType;
import com.google.common.collect.Multimap;
import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;

public class BundleUtils {
	private static final String SIZE = "Size", KEY = "Key", VALUE = "Value", NULL = "Null";

	private static final String IS_LOWER_BOUNDED = "IsLowerBounded", IS_UPPER_BOUNDED = "IsUpperBounded";
	private static final String LOWER_TYPE = "LowerType", LOWER_VALUE = "LowerValue", UPPER_TYPE = "UpperType", UPPER_VALUE = "UpperValue";

	private BundleUtils() {}

	public static <E extends Parcelable> void putParList(Bundle b, String key, List<E> value) {
		if (value == null) {
			b.putInt(key + SIZE, -1);
			return;
		}

		int n = value.size();
		b.putInt(key + SIZE, n);
		for (int i = 0; i < n; i++) {
			b.putParcelable(key + i, value.get(i));
		}
	}

	public static <K extends Parcelable, V extends Parcelable> void putParMap(Bundle b, String key, Map<K, V> value) {
		if (value == null) {
			b.putInt(key + SIZE, -1);
			return;
		}

		int n = value.size();
		b.putInt(key + SIZE, n);
		int i = 0;
		for (Map.Entry<K, V> entry : value.entrySet()) {
			b.putParcelable(key + KEY + i, entry.getKey());
			b.putParcelable(key + VALUE + i, entry.getValue());
			i++;
		}
	}

	public static <K extends Parcelable, V extends Parcelable> void putParMultimap(Bundle b, String key, Multimap<K, V> value) {
		if (value == null) {
			b.putInt(key + SIZE, -1);
			return;
		}

		int n = value.size();
		b.putInt(key + SIZE, n);
		int i = 0;
		for (Map.Entry<K, Collection<V>> entry : value.asMap().entrySet()) {
			b.putParcelable(key + KEY + i, entry.getKey());
			int j = 0;
			for (V v : entry.getValue()) {
				b.putParcelable(key + VALUE + i + j, v);
				j++;
			}
			b.putInt(key + KEY + SIZE + i, j);
			i++;
		}
	}

	public static <C extends Comparable<?> & Parcelable> void putParRange(Bundle b, String key, Range<C> value) {
		if (value == null) {
			b.putBoolean(key + NULL, true);
			return;
		} else {
			b.putBoolean(key + NULL, false);

			if (value.hasLowerBound()) {
				b.putBoolean(key + IS_LOWER_BOUNDED, true);
				b.putInt(key + LOWER_TYPE, value.lowerBoundType().ordinal());
				b.putParcelable(key + LOWER_VALUE, value.lowerEndpoint());
			} else {
				b.putBoolean(key + IS_LOWER_BOUNDED, false);
			}

			if (value.hasUpperBound()) {
				b.putBoolean(key + IS_UPPER_BOUNDED, true);
				b.putInt(key + UPPER_TYPE, value.upperBoundType().ordinal());
				b.putParcelable(key + UPPER_VALUE, value.upperEndpoint());
			} else {
				b.putBoolean(key + IS_UPPER_BOUNDED, false);
			}
		}
	}

	public static <K extends Comparable<?> & Parcelable, V extends Parcelable> void putParParRangeMap(Bundle b, String key, RangeMap<K, V> value) {
		if (value == null) {
			b.putInt(key + SIZE, -1);
			return;
		}

		int n = value.asMapOfRanges().size();
		b.putInt(key + SIZE, n);
		int i = 0;
		for (Map.Entry<Range<K>, V> entry : value.asMapOfRanges().entrySet()) {
			putParRange(b, key + KEY + i, entry.getKey());
			b.putParcelable(key + VALUE + i, entry.getValue());
			i++;
		}
	}
	
	public static void putIntRange(Bundle b, String key, Range<Integer> value) {
		if (value == null) {
			b.putBoolean(key + NULL, true);
			return;
		} else {
			b.putBoolean(key + NULL, false);

			if (value.hasLowerBound()) {
				b.putBoolean(key + IS_LOWER_BOUNDED, true);
				b.putInt(key + LOWER_TYPE, value.lowerBoundType().ordinal());
				b.putInt(key + LOWER_VALUE, value.lowerEndpoint());
			} else {
				b.putBoolean(key + IS_LOWER_BOUNDED, false);
			}

			if (value.hasUpperBound()) {
				b.putBoolean(key + IS_UPPER_BOUNDED, true);
				b.putInt(key + UPPER_TYPE, value.upperBoundType().ordinal());
				b.putInt(key + UPPER_VALUE, value.upperEndpoint());
			} else {
				b.putBoolean(key + IS_UPPER_BOUNDED, false);
			}
		}
	}

	public static <V extends Parcelable> void putIntParRangeMap(Bundle b, String key, RangeMap<Integer, V> value) {
		if (value == null) {
			b.putInt(key + SIZE, -1);
			return;
		}

		int n = value.asMapOfRanges().size();
		b.putInt(key + SIZE, n);
		int i = 0;
		for (Map.Entry<Range<Integer>, V> entry : value.asMapOfRanges().entrySet()) {
			putIntRange(b, key + KEY + i, entry.getKey());
			b.putParcelable(key + VALUE + i, entry.getValue());
			i++;
		}
	}

	public static <E extends Parcelable> void getParList(Bundle b, String key, List<E> outVal) {
		int m = outVal.size();
		int n = b.getInt(key + SIZE);
		int i = 0;

		for (; i < m && i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) b.getParcelable(key + i);
			outVal.set(i, elem);
		}
		for (; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) b.getParcelable(key + i);
			outVal.set(i, elem);
		}
		for (; i < m; i++) {
			outVal.remove(n);
		}
	}

	public static <K extends Parcelable, V extends Parcelable> void getParParMap(Bundle b, String key, Map<K, V> outVal) {
		int n = b.getInt(key + SIZE);

		outVal.clear();
		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) b.getParcelable(key + KEY + i);
			@SuppressWarnings("unchecked")
			V v = (V) b.getParcelable(key + VALUE + i);
			outVal.put(k, v);
		}
	}

	public static <K extends Parcelable, V extends Parcelable> void getParParMultimap(Bundle b, String key, Multimap<K, V> outVal) {
		int n = b.getInt(key + SIZE);

		outVal.clear();
		for (int i = 0; i < n; i++) {
			K k = b.getParcelable(key + KEY + i);
			int q = b.getInt(key + KEY + SIZE + i);
			for (int j = 0; j < q; j++) {
				V v = b.getParcelable(key + VALUE + i + j);
				outVal.put(k, v);
			}
		}
	}
	
	public static Range<Integer> getIntRange(Bundle b, String key) {
		if (b.getBoolean(key + NULL)) {
			return null;
		} else {
			boolean isLowerBounded = b.getBoolean(key + IS_LOWER_BOUNDED);
			boolean isUpperBounded = b.getBoolean(key + IS_UPPER_BOUNDED);

			if (isLowerBounded && isUpperBounded) {
				BoundType lowerType = BoundType.values()[b.getInt(key + LOWER_TYPE)];
				int lowerVal = b.getInt(key + LOWER_VALUE);
				BoundType upperType = BoundType.values()[b.getInt(key + UPPER_TYPE)];
				int upperVal = b.getInt(key + UPPER_VALUE);
				return Range.range(lowerVal, lowerType, upperVal, upperType);
			} else if (isLowerBounded) {
				BoundType lowerType = BoundType.values()[b.getInt(key + LOWER_TYPE)];
				int lowerVal = b.getInt(key + LOWER_VALUE);
				return Range.downTo(lowerVal, lowerType);
			} else if (isUpperBounded) {
				BoundType upperType = BoundType.values()[b.getInt(key + UPPER_TYPE)];
				int upperVal = b.getInt(key + UPPER_VALUE);
				return Range.upTo(upperVal, upperType);
			} else {
				return Range.all();
			}
		}
	}

	public static <V extends Parcelable> void getIntParRangeMap(Bundle b, String key, RangeMap<Integer, V> outVal) {
		int n = b.getInt(key + SIZE);

		outVal.clear();
		for (int i = 0; i < n; i++) {
			Range<Integer> range = getIntRange(b, key + KEY + i);
			@SuppressWarnings("unchecked")
			V value = (V) b.getParcelable(key + VALUE + i);
			outVal.put(range, value);
		}
	}

	public static <C extends Comparable<?> & Parcelable> Range<C> getParRange(Bundle b, String key) {
		if (b.getBoolean(key + NULL)) {
			return null;
		} else {
			boolean isLowerBounded = b.getBoolean(key + IS_LOWER_BOUNDED);
			boolean isUpperBounded = b.getBoolean(key + IS_UPPER_BOUNDED);

			if (isLowerBounded && isUpperBounded) {
				BoundType lowerType = BoundType.values()[b.getInt(key + LOWER_TYPE)];
				C lowerVal = b.getParcelable(key + LOWER_VALUE);
				BoundType upperType = BoundType.values()[b.getInt(key + UPPER_TYPE)];
				C upperVal = b.getParcelable(key + UPPER_VALUE);
				return Range.range(lowerVal, lowerType, upperVal, upperType);
			} else if (isLowerBounded) {
				BoundType lowerType = BoundType.values()[b.getInt(key + LOWER_TYPE)];
				C lowerVal = b.getParcelable(key + LOWER_VALUE);
				return Range.downTo(lowerVal, lowerType);
			} else if (isUpperBounded) {
				BoundType upperType = BoundType.values()[b.getInt(key + UPPER_TYPE)];
				C upperVal = b.getParcelable(key + UPPER_VALUE);
				return Range.upTo(upperVal, upperType);
			} else {
				return Range.all();
			}
		}
	}

	public static <K extends Comparable<?> & Parcelable, V extends Parcelable> void getParParRangeMap(Bundle b, String key, RangeMap<K, V> outVal) {
		int n = b.getInt(key + SIZE);

		outVal.clear();
		for (int i = 0; i < n; i++) {
			Range<K> range = getParRange(b, key + KEY + i);
			@SuppressWarnings("unchecked")
			V value = (V) b.getParcelable(key + VALUE + i);
			outVal.put(range, value);
		}
	}

}
