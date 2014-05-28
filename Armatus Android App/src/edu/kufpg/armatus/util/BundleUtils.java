package edu.kufpg.armatus.util;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.SortedSet;

import android.annotation.SuppressLint;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.os.Parcelable;
import android.util.Log;
import android.util.SparseArray;

import com.google.common.base.Optional;
import com.google.common.collect.BoundType;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableRangeMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;
import com.google.common.collect.SetMultimap;

public class BundleUtils {
	private static final String TAG = BundleUtils.class.getSimpleName();

	private static final String SIZE = "Size", KEY = "Key", VALUE = "Value", NULL = "Null", PRESENT_OR_NULL = "PresentOrNull";
	private static final String NAME = "Name", NO_NAME = "!@#$%^&*()";
	private static final String TYPE = "Type";
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
	private static final int VAL_OPTIONAL = 29;
	private static final int VAL_RANGE = 30;
	private static final int VAL_RANGEMAP = 31;
	private static final int VAL_SET = 32;
	//private static final int VAL_COLLECTION = 32;
	private static final int VAL_ENUM = 33;

	private BundleUtils() {}

	private static Class<?> forName(String className) throws IllegalArgumentException {
		try {
			return Class.forName(className);
		} catch (ClassNotFoundException e) {
			Log.e(TAG, "Illegal access when unmarshalling: "
					+ className, e);
			throw new IllegalArgumentException("ClassNotFoundException when unmarshalling: "
					+ className);
		}
	}

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

	private static <E, C extends Collection<E>> C addToCollection(Bundle b, String key, C outVal, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) getValue(b, key + i);
			outVal.add(elem);
		}
		return outVal;
	}

	private static <E, ICB extends ImmutableCollection.Builder<E>> void addToImmutableCollectionBuilder(Bundle b, String key, ICB builder, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) getValue(b, key + i);
			builder.add(elem);
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

	public static <E extends Enum<E>> E getEnum(Bundle b, String key) {
		String className = b.getString(key + NAME);
		if (className.equals(NO_NAME)) {
			return null;
		} else {
			@SuppressWarnings("unchecked")
			Class<E> cls = (Class<E>) forName(className);
			String valueName = b.getString(key);
			return Enum.valueOf(cls, valueName);
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
		return addToCollection(b, key, outVal, n);
	}

	public static <E> ImmutableList<E> getImmutableList(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		ImmutableList.Builder<E> builder = ImmutableList.builder();
		addToImmutableCollectionBuilder(b, key, builder, n);
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

	public static <T> Optional<T> getOptional(Bundle b, String key) {
		int s = b.getInt(key + PRESENT_OR_NULL);
		if (s == -1) {
			return null;
		} else if (s == 0) {
			return Optional.absent();
		} else {
			@SuppressWarnings("unchecked")
			T thing = (T) getValue(b, key);
			return Optional.of(thing);
		}
	}

	public static <C extends Comparable<? super C>> Range<C> getRange(Bundle b, String key) {
		if (b.getBoolean(key + NULL)) {
			return null;
		} else {
			boolean isLowerBounded = b.getBoolean(key + IS_LOWER_BOUNDED);
			boolean isUpperBounded = b.getBoolean(key + IS_UPPER_BOUNDED);

			if (isLowerBounded && isUpperBounded) {
				BoundType lowerType = getEnum(b, key + LOWER_TYPE);
				@SuppressWarnings("unchecked")
				C lowerVal = (C) getValue(b, key + LOWER_VALUE);

				BoundType upperType = getEnum(b, key + UPPER_TYPE);
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

	public static <K extends Comparable<? super K>, V> RangeMap<K, V> getRangeMap(Bundle b, String key) {
		String name = b.getString(key + NAME);
		if (isAssignableFrom(ImmutableRangeMap.class, name)) {
			return getImmutableRangeMap(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getRangeMapInternal(b, key);
		}
	}

	public static <K extends Comparable<? super K>, V> ImmutableRangeMap<K, V> getImmutableRangeMap(Bundle b, String key) {
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

	private static <K extends Comparable<? super K>, V> RangeMap<K, V> getRangeMapInternal(Bundle b, String key) {
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

	public static <E> Set<E> getSet(Bundle b, String key) {
		String name = b.getString(key + NAME);

		if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			@SuppressWarnings("unchecked")
			Set<E> set = (Set<E>) getImmutableSortedSet(b, key);
			return set;
		} else if (isAssignableFrom(ImmutableSet.class, name)) {
			return getImmutableSet(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getSetInternal(b, key);
		}
	}

	private static <E> Set<E> getSetInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		Set<E> outVal = (Set<E>) newInstance(name);
		return addToCollection(b, key, outVal, n);
	}

	public static <E extends Comparable<? super E>> SortedSet<E> getSortedSet(Bundle b, String key) {
		String name = b.getString(key + NAME);

		if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return getImmutableSortedSet(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getSortedSetInternal(b, key);
		}
	}

	private static <E> SortedSet<E> getSortedSetInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		SortedSet<E> outVal = (SortedSet<E>) newInstance(name);
		return addToCollection(b, key, outVal, n);
	}

	public static <E extends Comparable<? super E>> NavigableSet<E> getNavigableSet(Bundle b, String key) {
		String name = b.getString(key + NAME);

		if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return getImmutableSortedSet(b, key);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return getNavigableSetInternal(b, key);
		}
	}

	private static <E> NavigableSet<E> getNavigableSetInternal(Bundle b, String key) {
		int n = b.getInt(key + SIZE);
		String name = b.getString(key + NAME);
		@SuppressWarnings("unchecked")
		NavigableSet<E> outVal = (NavigableSet<E>) newInstance(name);
		return addToCollection(b, key, outVal, n);
	}

	public static <E> ImmutableSet<E> getImmutableSet(Bundle b, String key) {
		if (b.getString(key + NAME).equals(NO_NAME)) {
			return null;
		} else {
			int n = b.getInt(key + SIZE);
			ImmutableSet.Builder<E> builder = ImmutableSet.builder();
			addToImmutableCollectionBuilder(b, key, builder, n);
			return builder.build();
		}
	}

	public static <E extends Comparable<? super E>> ImmutableSortedSet<E> getImmutableSortedSet(Bundle b, String key) {
		if (b.getString(key + NAME).equals(NO_NAME)) {
			return null;
		} else {
			int n = b.getInt(key + SIZE);
			ImmutableSortedSet.Builder<E> builder = ImmutableSortedSet.naturalOrder();
			addToImmutableCollectionBuilder(b, key, builder, n);
			return builder.build();
		}
	}

	@SuppressLint("NewApi")
	public static final Object getValue(Bundle b, String key) {
		int type = b.getInt(key + TYPE);

		switch (type) {
		case VAL_NULL:
			return null;	
		case VAL_ENUM:
			return getEnum(b, key);
		case VAL_OPTIONAL:
			return getOptional(b, key);
		case VAL_STRING:
			return b.getString(key);
		case VAL_INTEGER:
			return b.getInt(key);
		case VAL_MULTIMAP:
			return getMultimap(b, key);
		case VAL_RANGE:
			return getRange(b, key);
		case VAL_RANGEMAP:
			return getRangeMap(b, key);
		case VAL_MAP:
			return getMap(b, key);
		case VAL_PARCELABLE:
			return b.getParcelable(key);
		case VAL_SHORT:
			return b.getShort(key);
		case VAL_LONG:
			return b.getLong(key);
		case VAL_FLOAT:
			return b.getFloat(key);
		case VAL_DOUBLE:
			return b.getDouble(key);
		case VAL_BOOLEAN:
			return b.getBoolean(key);
		case VAL_CHARSEQUENCE:
			return b.getCharSequence(key);
		case VAL_LIST:
			return getList(b, key);		
		case VAL_SET:
			return getSet(b, key);
		case VAL_BOOLEANARRAY:
			return b.getBooleanArray(key);        
		case VAL_BYTEARRAY:
			return b.getByteArray(key);
		case VAL_STRINGARRAY:
			return b.getStringArray(key);
		case VAL_CHARSEQUENCEARRAY:
			return b.getCharSequenceArray(key);
		case VAL_CHARARRAY:
			return b.getChar(key);
		case VAL_DOUBLEARRAY:
			return b.getDoubleArray(key);
		case VAL_FLOATARRAY:
			return b.getFloatArray(key);
		case VAL_SHORTARRAY:
			return b.getShortArray(key);
		case VAL_IBINDER:
			return b.getBinder(key);
		case VAL_INTARRAY:
			return b.getIntArray(key);
		case VAL_LONGARRAY:
			return b.getLongArray(key);
		case VAL_OBJECTARRAY:
			return getArray(b, key);
		case VAL_BYTE:
			return b.getByte(key);
		case VAL_SERIALIZABLE:
			return b.getSerializable(key);
		case VAL_PARCELABLEARRAY:
			return b.getParcelableArray(key);
		case VAL_SPARSEPARCELABLEARRAY:
			return b.getSparseParcelableArray(key);
		case VAL_BUNDLE:
			return b.getBundle(key); // loading will be deferred
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

	public static <E extends Enum<?>> void putEnum(Bundle b, String key, E value) {
		if (value == null) {
			b.putString(key + NAME, NO_NAME);
		} else {
			b.putString(key + NAME, value.getDeclaringClass().getName());
			b.putString(key, value.name());
		}
	}

	public static <E> void putList(Bundle b, String key, List<E> value) {
		if (value == null) {
			b.putString(key + NAME, NO_NAME);
		} else {
			b.putString(key + NAME, value.getClass().getName());
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
			b.putString(key + NAME, value.getClass().getName());
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
			b.putString(key + NAME, value.getClass().getName());
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

	public static <T> void putOptional(Bundle b, String key, Optional<T> value) {
		if (value == null) {
			b.putInt(key + PRESENT_OR_NULL, -1);
		} else if (value.isPresent()) {
			b.putInt(key + PRESENT_OR_NULL, 1);
			putValue(b, key, value.get());
		} else {
			b.putInt(key + PRESENT_OR_NULL, 0);
		}
	}

	public static <C extends Comparable<?>> void putRange(Bundle b, String key, Range<C> value) {
		if (value == null) {
			b.putBoolean(key + NULL, true);
		} else {
			b.putBoolean(key + NULL, false);

			if (value.hasLowerBound()) {
				b.putBoolean(key + IS_LOWER_BOUNDED, true);
				putEnum(b, key + LOWER_TYPE, value.lowerBoundType());
				putValue(b, key + LOWER_VALUE, value.lowerEndpoint());
			} else {
				b.putBoolean(key + IS_LOWER_BOUNDED, false);
			}

			if (value.hasUpperBound()) {
				b.putBoolean(key + IS_UPPER_BOUNDED, true);
				putEnum(b, key + UPPER_TYPE, value.upperBoundType());
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
			b.putString(key + NAME, value.getClass().getName());
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

	public static <E> void putSet(Bundle b, String key, Set<E> value) {
		if (value == null) {
			b.putString(key + NAME, NO_NAME);
		} else {
			b.putString(key + NAME, value.getClass().getName());
			int n = value.size();
			b.putInt(key + SIZE, n);
			int i = 0;
			for (E elem : value) {
				putValue(b, key + i, elem);
				i++;
			}
		}
	}

	@SuppressLint("NewApi")
	public static void putValue(Bundle b, String key, Object v) {
		String keyType = key + TYPE;

		if (v == null) {
			b.putInt(keyType, VAL_NULL);
		} else if (v.getClass().isEnum()) {
			b.putInt(keyType, VAL_ENUM);
			putEnum(b, key, (Enum<?>) v);
		} else if (v instanceof Optional) {
			b.putInt(keyType, VAL_OPTIONAL);
			putOptional(b, key, (Optional<?>) v);
		} else if (v instanceof String) {
			b.putInt(keyType, VAL_STRING);
			b.putString(key, (String) v);
		} else if (v instanceof Integer) {
			b.putInt(keyType, VAL_INTEGER);
			b.putInt(key, (Integer) v);
		} else if (v instanceof Multimap) {
			b.putInt(keyType, VAL_MULTIMAP);
			putMultimap(b, key, (Multimap<?,?>) v);
		} else if (v instanceof Range) {
			b.putInt(keyType, VAL_RANGE);
			@SuppressWarnings("unchecked")
			Range<? extends Comparable<?>> range = (Range<? extends Comparable<?>>) v;
			putRange(b, key, range);
		} else if (v instanceof RangeMap) {
			b.putInt(keyType, VAL_RANGEMAP);
			@SuppressWarnings("unchecked")
			RangeMap<? extends Comparable<?>, ?> rangeMap = (RangeMap<? extends Comparable<?>, ?>) v;
			putRangeMap(b, key, rangeMap);
		} else if (v instanceof Map) {
			b.putInt(keyType, VAL_MAP);
			putMap(b, key, (Map<?,?>) v);
		} else if (v instanceof Bundle) {
			// Must be before Parcelable
			b.putInt(keyType, VAL_BUNDLE);
			b.putBundle(key, (Bundle) v);
		} else if (v instanceof Parcelable) {
			b.putInt(keyType, VAL_PARCELABLE);
			b.putParcelable(key, (Parcelable) v);
		} else if (v instanceof Short) {
			b.putInt(keyType, VAL_SHORT);
			b.putShort(key, (Short) v);
		} else if (v instanceof Long) {
			b.putInt(keyType, VAL_LONG);
			b.putLong(key, (Long) v);
		} else if (v instanceof Float) {
			b.putInt(keyType, VAL_FLOAT);
			b.putFloat(key, (Float) v);
		} else if (v instanceof Double) {
			b.putInt(keyType, VAL_DOUBLE);
			b.putDouble(key, (Double) v);
		} else if (v instanceof Boolean) {
			b.putInt(keyType, VAL_BOOLEAN);
			b.putBoolean(key, (Boolean) v);
		} else if (v instanceof CharSequence) {
			// Must be after String
			b.putInt(keyType, VAL_CHARSEQUENCE);
			b.putCharSequence(key, (CharSequence) v);
		} else if (v instanceof List) {
			b.putInt(keyType, VAL_LIST);
			putList(b, key, (List<?>) v);
		} else if (v instanceof Set) {
			b.putInt(keyType, VAL_SET);
			putSet(b, key, (Set<?>) v);
		} else if (v instanceof SparseArray) {
			b.putInt(keyType, VAL_SPARSEPARCELABLEARRAY);
			@SuppressWarnings("unchecked")
			SparseArray<? extends Parcelable> spa = (SparseArray<? extends Parcelable>) v;
			b.putSparseParcelableArray(key, spa);
		} else if (v instanceof boolean[]) {
			b.putInt(keyType, VAL_BOOLEANARRAY);
			b.putBooleanArray(key, (boolean[]) v);
		} else if (v instanceof byte[]) {
			b.putInt(keyType, VAL_BYTEARRAY);
			b.putByteArray(key, (byte[]) v);
		} else if (v instanceof String[]) {
			b.putInt(keyType, VAL_STRINGARRAY);
			b.putStringArray(key, (String[]) v);
		} else if (v instanceof char[]) {
			b.putInt(keyType, VAL_CHARARRAY);
			b.putCharArray(key, (char[]) v);
		} else if (v instanceof double[]) {
			b.putInt(keyType, VAL_DOUBLEARRAY);
			b.putDoubleArray(key, (double[]) v);
		} else if (v instanceof float[]) {
			b.putInt(keyType, VAL_FLOATARRAY);
			b.putFloatArray(key, (float[]) v);
		} else if (v instanceof short[]) {
			b.putInt(keyType, VAL_SHORTARRAY);
			b.putShortArray(key, (short[]) v);
		} else if (v instanceof CharSequence[]) {
			// Must be after String[] and before Object[]
			b.putInt(keyType, VAL_CHARSEQUENCEARRAY);
			b.putCharSequenceArray(key, (CharSequence[]) v);
		} else if (v instanceof IBinder && Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
			b.putInt(keyType, VAL_IBINDER);
			b.putBinder(key, (IBinder) v);
		} else if (v instanceof Parcelable[]) {
			b.putInt(keyType, VAL_PARCELABLEARRAY);
			b.putParcelableArray(key, (Parcelable[]) v);
		} else if (v instanceof int[]) {
			b.putInt(keyType, VAL_INTARRAY);
			b.putIntArray(key, (int[]) v);
		} else if (v instanceof long[]) {
			b.putInt(keyType, VAL_LONGARRAY);
			b.putLongArray(key, (long[]) v);
		} else if (v instanceof Object[]) {
			b.putInt(keyType, VAL_OBJECTARRAY);
			putArray(b, key, (Object[]) v);
		} else if (v instanceof Byte) {
			b.putInt(keyType, VAL_BYTE);
			b.putByte(key, (Byte) v);
		} else if (v instanceof Serializable) {
			// Must be last
			b.putInt(keyType, VAL_SERIALIZABLE);
			b.putSerializable(key, (Serializable) v);
		} else {
			throw new RuntimeException("Parcel: unable to marshal value " + v);
		}
	}

}
