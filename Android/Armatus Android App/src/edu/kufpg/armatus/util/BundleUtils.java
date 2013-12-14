package edu.kufpg.armatus.util;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import android.annotation.SuppressLint;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Parcelable;
import android.util.Log;
import android.util.SparseArray;

import com.google.common.collect.BoundType;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableRangeMap;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;
import com.google.common.collect.SetMultimap;

public class BundleUtils {
	private static final String TAG = BundleUtils.class.getName();

	private static final String SIZE = "Size", KEY = "Key", VALUE = "Value", NULL = "Null";
	private static final String NAME = "Name", NO_NAME = "!@#$%^&*()";
	private static final String IS_LOWER_BOUNDED = "IsLowerBounded", IS_UPPER_BOUNDED = "IsUpperBounded";
	private static final String LOWER_TYPE = "LowerType", LOWER_VALUE = "LowerValue", UPPER_TYPE = "UpperType", UPPER_VALUE = "UpperValue";
	private static final String CREATE = "create";

	private static final int VAL_NULL = -1;
	private static final int VAL_STRING = 0;
	private static final int VAL_INTEGER = 1;
	private static final int VAL_MAP = 2;
	private static final int VAL_BUNDLE = 3;
	private static final int VAL_PARCELABLE = 4;
	private static final int VAL_SHORT = 5;
	private static final int VAL_LONG = 6;
	private static final int VAL_FLOAT = 7;
	private static final int VAL_DOUBLE = 8;
	private static final int VAL_BOOLEAN = 9;
	private static final int VAL_CHARSEQUENCE = 10;
	private static final int VAL_LIST  = 11;
	private static final int VAL_SPARSEPARCELABLEARRAY = 12;
	private static final int VAL_BYTEARRAY = 13;
	private static final int VAL_STRINGARRAY = 14;
	private static final int VAL_IBINDER = 15;
	private static final int VAL_PARCELABLEARRAY = 16;
	private static final int VAL_OBJECTARRAY = 17;
	private static final int VAL_INTARRAY = 18;
	private static final int VAL_LONGARRAY = 19;
	private static final int VAL_BYTE = 20;
	private static final int VAL_SERIALIZABLE = 21;
	private static final int VAL_BOOLEANARRAY = 22;
	private static final int VAL_CHARSEQUENCEARRAY = 23;
	private static final int VAL_CHARARRAY = 24;
	private static final int VAL_DOUBLEARRAY = 25;
	private static final int VAL_FLOATARRAY = 26;
	private static final int VAL_SHORTARRAY = 27;
	private static final int VAL_MULTIMAP = 28;
	private static final int VAL_RANGE = 29;
	private static final int VAL_RANGEMAP = 30;

	private BundleUtils() {}

	private static boolean isAssignableFrom(Class<?> cls, String asgnName) throws IllegalArgumentException {
		try {
			return cls.isAssignableFrom(Class.forName(asgnName));
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException();
		}
	}

	private static Object newInstance(String name) throws IllegalArgumentException {
		try {
			Class<?> c = Class.forName(name);
			return c.newInstance();
		} catch (ClassNotFoundException e) {
			Log.e(TAG, "Illegal access when unmarshalling: "
					+ name, e);
			throw new IllegalArgumentException("ClassNotFoundException when unmarshalling: "
					+ name);
		} catch (InstantiationException e) {
			Log.e(TAG, "Class not found when unmarshalling: "
					+ name, e);
			throw new IllegalArgumentException("InstantiationException when unmarshalling: "
					+ name);
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException("IllegalAccessException when unmarshalling: "
					+ name);
		}
	}

	private static Object singletonInstance(String className, String singletonMethodName) throws IllegalArgumentException {
		try {
			Class<?> c = Class.forName(className);
			Method m = c.getMethod(singletonMethodName);
			return m.invoke(null);
		} catch (ClassNotFoundException e) {
			Log.e(TAG, "Illegal access when unmarshalling: "
					+ className + " with method " + singletonMethodName, e);
			throw new IllegalArgumentException("ClassNotFoundException when unmarshalling: "
					+ className + " with method " + singletonMethodName);
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException("IllegalAccessException when unmarshalling: "
					+ className + " with method " + singletonMethodName);
		} catch (NoSuchMethodException e) {
			Log.e(TAG, "Method not found when unmarshalling: "
					+ className + " with method " + singletonMethodName, e);
			throw new IllegalArgumentException("NoSuchMethodException when unmarshalling: "
					+ className + " with method " + singletonMethodName);
		} catch (InvocationTargetException e) {
			throw new IllegalArgumentException("InvocationTargetException when unmarshalling: "
					+ className + " with method " + singletonMethodName);
		}
	}
	
	public static <T> T[] getArray(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		if (n < 0) {
			return null;
		} else {
			Object[] outVal = new Object[n];
			for (int i = 0; i < n; i++) {
				@SuppressWarnings("unchecked")
				T t = (T) getValue(b, key + i);
				outVal[i] = t;
			}

			@SuppressWarnings("unchecked")
			T[] ts = (T[]) outVal;
			return ts;
		}
	}

	public static <E> List<E> getList(Bundle b, String key) {
		String name = b.getString(key + NAME);

		if (isAssignableFrom(ImmutableList.class, name)) {
			return getImmutableList(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getListInternal(b, key);
		}
	}

	private static <E> List<E> getListInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		List<E> outVal = (List<E>) newInstance(name);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) getValue(b, key + i);
			outVal.add(elem);
		}
		return outVal;
	}

	public static <E> ImmutableList<E> getImmutableList(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		ImmutableList.Builder<E> builder = ImmutableList.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) getValue(b, key + i);
			builder.add(elem);
		}
		return builder.build();
	}

	public static <K, V> Map<K, V> getMap(Bundle b, String key) {
		String name = b.getString(key + NAME);

		if (isAssignableFrom(ImmutableMap.class, name)) {
			return getImmutableMap(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getMapInternal(b, key);
		}
	}

	private static <K, V> Map<K, V> getMapInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		Map<K, V> outVal = (Map<K, V>) newInstance(name);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) getValue(b, key + KEY + i);
			@SuppressWarnings("unchecked")
			V v = (V) getValue(b, key + VALUE + i);
			outVal.put(k, v);
		}

		return outVal;
	}

	public static <K, V> ImmutableMap<K, V> getImmutableMap(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		ImmutableMap.Builder<K, V> builder = ImmutableMap.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) getValue(b, key + KEY + i);
			@SuppressWarnings("unchecked")
			V v = (V) getValue(b, key + VALUE + i);
			builder.put(k, v);
		}

		return builder.build();
	}

	public static <K, V> Multimap<K, V> getMultimap(Bundle b, String key) {
		String name = b.getString(key + NAME);

		if (isAssignableFrom(ImmutableListMultimap.class, name)) {
			return getImmutableListMultimap(b, key);
		} else if (isAssignableFrom(ImmutableSetMultimap.class, name)) {
			return getImmutableSetMultimap(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getMultimapInternal(b, key);
		}
	}

	public static <K, V> ListMultimap<K, V> getListMultimap(Bundle b, String key) {
		String name = b.getString(key + NAME);

		if (isAssignableFrom(ImmutableListMultimap.class, name)) {
			return getImmutableListMultimap(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getListMultimapInternal(b, key);
		}
	}

	public static <K, V> SetMultimap<K, V> getSetMultimap(Bundle b, String key) {
		String name = b.getString(key + NAME);

		if (isAssignableFrom(ImmutableSetMultimap.class, name)) {
			return getImmutableSetMultimap(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getSetMultimapInternal(b, key);
		}
	}

	public static <K, V> ImmutableListMultimap<K, V> getImmutableListMultimap(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		ImmutableListMultimap.Builder<K, V> builder = ImmutableListMultimap.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) getValue(b, key + KEY + i);
			int q = b.getInt(key + KEY + SIZE + i);
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) getValue(b, key + VALUE + i + j);
				builder.put(k, v);
			}
		}

		return builder.build();
	}

	public static <K, V> ImmutableSetMultimap<K, V> getImmutableSetMultimap(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		ImmutableSetMultimap.Builder<K, V> builder = ImmutableSetMultimap.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) getValue(b, key + KEY + i);
			int q = b.getInt(key + KEY + SIZE + i);
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) getValue(b, key + VALUE + i + j);
				builder.put(k, v);
			}
		}

		return builder.build();
	}

	private static <K, V> Multimap<K, V> getMultimapInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		Multimap<K, V> outVal = (Multimap<K, V>) singletonInstance(name, CREATE);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) getValue(b, key + KEY + i);
			int q = b.getInt(key + KEY + SIZE + i);
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) getValue(b, key + VALUE + i + j);
				outVal.put(k, v);
			}
		}

		return outVal;
	}

	private static <K, V> ListMultimap<K, V> getListMultimapInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		ListMultimap<K, V> outVal = (ListMultimap<K, V>) singletonInstance(name, CREATE);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) getValue(b, key + KEY + i);
			int q = b.getInt(key + KEY + SIZE + i);
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) getValue(b, key + VALUE + i + j);
				outVal.put(k, v);
			}
		}

		return outVal;
	}

	private static <K, V> SetMultimap<K, V> getSetMultimapInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		SetMultimap<K, V> outVal = (SetMultimap<K, V>) singletonInstance(name, CREATE);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) getValue(b, key + KEY + i);
			int q = b.getInt(key + KEY + SIZE + i);
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) getValue(b, key + VALUE + i + j);
				outVal.put(k, v);
			}
		}

		return outVal;
	}

	public static <C extends Comparable<?>> Range<C> getRange(Bundle b, String key) {
		if (b.getBoolean(key + NULL)) {
			return null;
		} else {
			boolean isLowerBounded = b.getBoolean(key + IS_LOWER_BOUNDED);
			boolean isUpperBounded = b.getBoolean(key + IS_UPPER_BOUNDED);

			if (isLowerBounded && isUpperBounded) {
				BoundType lowerType = BoundType.values()[b.getInt(key + LOWER_TYPE)];
				@SuppressWarnings("unchecked")
				C lowerVal = (C) getValue(b, key + LOWER_VALUE);

				BoundType upperType = BoundType.values()[b.getInt(key + UPPER_TYPE)];
				@SuppressWarnings("unchecked")
				C upperVal = (C) getValue(b, key + UPPER_VALUE);

				return Range.range(lowerVal, lowerType, upperVal, upperType);
			} else if (isLowerBounded) {
				BoundType lowerType = BoundType.values()[b.getInt(key + LOWER_TYPE)];
				@SuppressWarnings("unchecked")
				C lowerVal = (C) getValue(b, key + LOWER_VALUE);

				return Range.downTo(lowerVal, lowerType);
			} else if (isUpperBounded) {
				BoundType upperType = BoundType.values()[b.getInt(key + UPPER_TYPE)];
				@SuppressWarnings("unchecked")
				C upperVal = (C) getValue(b, key + UPPER_VALUE);

				return Range.upTo(upperVal, upperType);
			} else {
				return Range.all();
			}
		}
	}

	public static <K extends Comparable<?>, V> RangeMap<K, V> getRangeMap(Bundle b, String key) {
		String name = b.getString(key + NAME);
		if (isAssignableFrom(ImmutableRangeMap.class, name)) {
			return getImmutableRangeMap(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getRangeMapInternal(b, key);
		}
	}

	public static <K extends Comparable<?>, V> ImmutableRangeMap<K, V> getImmutableRangeMap(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		ImmutableRangeMap.Builder<K, V> builder = ImmutableRangeMap.builder();

		for (int i = 0; i < n; i++) {
			Range<K> range = getRange(b, key + KEY + i);
			@SuppressWarnings("unchecked")
			V value = (V) getValue(b, key + VALUE + i);
			builder.put(range, value);
		}

		return builder.build();
	}

	private static <K extends Comparable<?>, V> RangeMap<K, V> getRangeMapInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		RangeMap<K, V> outVal = (RangeMap<K, V>) singletonInstance(name, CREATE);

		for (int i = 0; i < n; i++) {
			Range<K> range = getRange(b, key + KEY + i);
			@SuppressWarnings("unchecked")
			V value = (V) getValue(b, key + VALUE + i);
			outVal.put(range, value);
		}

		return outVal;
	}

	@SuppressLint("NewApi")
	public static final Object getValue(Bundle b, String key) {
		int type = b.getInt(key + KEY);
		String kv = key + VALUE;

		switch (type) {
		case VAL_NULL:
			return null;

		case VAL_STRING:
			return b.getString(kv);

		case VAL_INTEGER:
			return b.getInt(kv);

		case VAL_MULTIMAP:
			return getMultimap(b, kv);

		case VAL_RANGE:
			return getRange(b, kv);

		case VAL_RANGEMAP:
			return getRangeMap(b, kv);

		case VAL_MAP:
			return getMap(b, kv);

		case VAL_PARCELABLE:
			return b.getParcelable(kv);

		case VAL_SHORT:
			return b.getShort(kv);

		case VAL_LONG:
			return b.getLong(kv);

		case VAL_FLOAT:
			return b.getFloat(kv);

		case VAL_DOUBLE:
			return b.getDouble(kv);

		case VAL_BOOLEAN:
			return b.getBoolean(kv);

		case VAL_CHARSEQUENCE:
			return b.getCharSequence(kv);

		case VAL_LIST:
			return getList(b, kv);

		case VAL_BOOLEANARRAY:
			return b.getBooleanArray(kv);        

		case VAL_BYTEARRAY:
			return b.getByteArray(kv);

		case VAL_STRINGARRAY:
			return b.getStringArray(kv);

		case VAL_CHARSEQUENCEARRAY:
			return b.getCharSequenceArray(kv);

		case VAL_CHARARRAY:
			return b.getChar(kv);

		case VAL_DOUBLEARRAY:
			return b.getDoubleArray(kv);

		case VAL_FLOATARRAY:
			return b.getFloatArray(kv);

		case VAL_SHORTARRAY:
			return b.getShortArray(kv);

		case VAL_IBINDER:
			return b.getBinder(kv);

		case VAL_INTARRAY:
			return b.getIntArray(kv);

		case VAL_LONGARRAY:
			return b.getLongArray(kv);

		case VAL_OBJECTARRAY:
			return getArray(b, kv);

		case VAL_BYTE:
			return b.getByte(kv);

		case VAL_SERIALIZABLE:
			return b.getSerializable(kv);

		case VAL_PARCELABLEARRAY:
			return b.getParcelableArray(kv);

		case VAL_SPARSEPARCELABLEARRAY:
			return b.getSparseParcelableArray(kv);

		case VAL_BUNDLE:
			return b.getBundle(kv); // loading will be deferred

		default:
			throw new RuntimeException(
					"Parcel " + b + ": Unmarshalling unknown type code " + type);
		}
	}

	public static <T> void putArray(Bundle b, String key, T[] value) {
		if (value == null) {
			b.putInt(key + SIZE, -1);
		} else {
			int n = value.length;
			b.putInt(key + SIZE, n);
			for (int i = 0; i < n; i++) {
				putValue(b, key + i, value[i]);
			}
		}
	}

	public static <E> void putList(Bundle b, String key, List<E> value) {
		if (value == null) {
			b.putString(key + NAME, NO_NAME);
		} else {
			b.putString(key + NAME, value.getClass().getCanonicalName());
			int n = value.size();
			b.putInt(key + SIZE, n);
			for (int i = 0; i < n; i++) {
				putValue(b, key + i, value.get(i));
			}
		}
	}

	public static <K, V> void putMap(Bundle b, String key, Map<K, V> value) {
		if (value == null) {
			b.putString(key + NAME, NO_NAME);
		} else {
			b.putString(key + NAME, value.getClass().getCanonicalName());
			int n = value.size();
			b.putInt(key + SIZE, n);
			int i = 0;
			for (Map.Entry<K, V> entry : value.entrySet()) {
				putValue(b, key + KEY + i, entry.getKey());
				putValue(b, key + VALUE + i, entry.getValue());
				i++;
			}
		}
	}

	public static <K, V> void putMultimap(Bundle b, String key, Multimap<K, V> value) {
		if (value == null) {
			b.putString(key + NAME, NO_NAME);
		} else {
			b.putString(key + NAME, value.getClass().getCanonicalName());
			int n = value.size();
			b.putInt(key + SIZE, n);
			int i = 0;
			for (Map.Entry<K, Collection<V>> entry : value.asMap().entrySet()) {
				putValue(b, key + KEY + i, entry.getKey());
				int j = 0;
				for (V v : entry.getValue()) {
					putValue(b, key + VALUE + i + j, v);
					j++;
				}
				b.putInt(key + KEY + SIZE + i, j);
				i++;
			}
		}
	}

	public static <C extends Comparable<?>> void putRange(Bundle b, String key, Range<C> value) {
		if (value == null) {
			b.putBoolean(key + NULL, true);
		} else {
			b.putBoolean(key + NULL, false);

			if (value.hasLowerBound()) {
				b.putBoolean(key + IS_LOWER_BOUNDED, true);
				b.putInt(key + LOWER_TYPE, value.lowerBoundType().ordinal());
				putValue(b, key + LOWER_VALUE, value.lowerEndpoint());
			} else {
				b.putBoolean(key + IS_LOWER_BOUNDED, false);
			}

			if (value.hasUpperBound()) {
				b.putBoolean(key + IS_UPPER_BOUNDED, true);
				b.putInt(key + UPPER_TYPE, value.upperBoundType().ordinal());
				putValue(b, key + UPPER_VALUE, value.upperEndpoint());
			} else {
				b.putBoolean(key + IS_UPPER_BOUNDED, false);
			}
		}
	}

	public static <K extends Comparable<?>, V> void putRangeMap(Bundle b, String key, RangeMap<K, V> value) {
		if (value == null) {
			b.putString(key + NAME, NO_NAME);
		} else {
			b.putString(key + NAME, value.getClass().getCanonicalName());
			int n = value.asMapOfRanges().size();
			b.putInt(key + SIZE, n);
			int i = 0;
			for (Map.Entry<Range<K>, V> entry : value.asMapOfRanges().entrySet()) {
				putRange(b, key + KEY + i, entry.getKey());
				putValue(b, key + VALUE + i, entry.getValue());
				i++;
			}
		}
	}

	@SuppressLint("NewApi")
	public static void putValue(Bundle b, String key, Object v) {
		String kk = key + KEY, kv = key + VALUE;

		if (v == null) {
			b.putInt(kk, VAL_NULL);
		} else if (v instanceof String) {
			b.putInt(kk, VAL_STRING);
			b.putString(kv, (String) v);
		} else if (v instanceof Integer) {
			b.putInt(kk, VAL_INTEGER);
			b.putInt(kv, (Integer) v);
		} else if (v instanceof Multimap) {
			b.putInt(kk, VAL_MULTIMAP);
			putMultimap(b, kv, (Multimap<?,?>) v);
		} else if (v instanceof Range) {
			b.putInt(kk, VAL_RANGE);
			@SuppressWarnings("unchecked")
			Range<? extends Comparable<?>> range = (Range<? extends Comparable<?>>) v;
			putRange(b, kv, range);
		} else if (v instanceof RangeMap) {
			b.putInt(kk, VAL_RANGEMAP);
			@SuppressWarnings("unchecked")
			RangeMap<? extends Comparable<?>, ?> rangeMap = (RangeMap<? extends Comparable<?>, ?>) v;
			putRangeMap(b, kv, rangeMap);
		} else if (v instanceof Map) {
			b.putInt(kk, VAL_MAP);
			putMap(b, kv, (Map<?,?>) v);
		} else if (v instanceof Bundle) {
			// Must be before Parcelable
			b.putInt(kk, VAL_BUNDLE);
			b.putBundle(kv, (Bundle) v);
		} else if (v instanceof Parcelable) {
			b.putInt(kk, VAL_PARCELABLE);
			b.putParcelable(kv, (Parcelable) v);
		} else if (v instanceof Short) {
			b.putInt(kk, VAL_SHORT);
			b.putShort(kv, (Short) v);
		} else if (v instanceof Long) {
			b.putInt(kk, VAL_LONG);
			b.putLong(kv, (Long) v);
		} else if (v instanceof Float) {
			b.putInt(kk, VAL_FLOAT);
			b.putFloat(kv, (Float) v);
		} else if (v instanceof Double) {
			b.putInt(kk, VAL_DOUBLE);
			b.putDouble(kv, (Double) v);
		} else if (v instanceof Boolean) {
			b.putInt(kk, VAL_BOOLEAN);
			b.putBoolean(kv, (Boolean) v);
		} else if (v instanceof CharSequence) {
			// Must be after String
			b.putInt(kk, VAL_CHARSEQUENCE);
			b.putCharSequence(kv, (CharSequence) v);
		} else if (v instanceof List) {
			b.putInt(kk, VAL_LIST);
			putList(b, kv, (List<?>) v);
		} else if (v instanceof SparseArray) {
			b.putInt(kk, VAL_SPARSEPARCELABLEARRAY);
			@SuppressWarnings("unchecked")
			SparseArray<? extends Parcelable> spa = (SparseArray<? extends Parcelable>) v;
			b.putSparseParcelableArray(kv, spa);
		} else if (v instanceof boolean[]) {
			b.putInt(kk, VAL_BOOLEANARRAY);
			b.putBooleanArray(kv, (boolean[]) v);
		} else if (v instanceof byte[]) {
			b.putInt(kk, VAL_BYTEARRAY);
			b.putByteArray(kv, (byte[]) v);
		} else if (v instanceof String[]) {
			b.putInt(kk, VAL_STRINGARRAY);
			b.putStringArray(kv, (String[]) v);
		} else if (v instanceof char[]) {
			b.putInt(kk, VAL_CHARARRAY);
			b.putCharArray(kv, (char[]) v);
		} else if (v instanceof double[]) {
			b.putInt(kk, VAL_DOUBLEARRAY);
			b.putDoubleArray(kv, (double[]) v);
		} else if (v instanceof float[]) {
			b.putInt(kk, VAL_FLOATARRAY);
			b.putFloatArray(kv, (float[]) v);
		} else if (v instanceof short[]) {
			b.putInt(kk, VAL_SHORTARRAY);
			b.putShortArray(kv, (short[]) v);
		} else if (v instanceof CharSequence[]) {
			// Must be after String[] and before Object[]
			b.putInt(kk, VAL_CHARSEQUENCEARRAY);
			b.putCharSequenceArray(kv, (CharSequence[]) v);
		} else if (v instanceof IBinder && Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
			b.putInt(kk, VAL_IBINDER);
			b.putBinder(kv, (IBinder) v);
		} else if (v instanceof Parcelable[]) {
			b.putInt(kk, VAL_PARCELABLEARRAY);
			b.putParcelableArray(kv, (Parcelable[]) v);
		} else if (v instanceof int[]) {
			b.putInt(kk, VAL_INTARRAY);
			b.putIntArray(kv, (int[]) v);
		} else if (v instanceof long[]) {
			b.putInt(kk, VAL_LONGARRAY);
			b.putLongArray(kv, (long[]) v);
		} else if (v instanceof Object[]) {
			b.putInt(kk, VAL_OBJECTARRAY);
			putArray(b, kv, (Object[]) v);
		} else if (v instanceof Byte) {
			b.putInt(kk, VAL_BYTE);
			b.putByte(kv, (Byte) v);
		} else if (v instanceof Serializable) {
			// Must be last
			b.putInt(kk, VAL_SERIALIZABLE);
			b.putSerializable(kv, (Serializable) v);
		} else {
			throw new RuntimeException("Parcel: unable to marshal value " + v);
		}
	}

}
