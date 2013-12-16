package edu.kufpg.armatus.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.collections4.Trie;

import android.os.Parcel;
import android.util.Log;

import com.google.common.base.Optional;
import com.google.common.collect.BoundType;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableRangeMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.SortedSetMultimap;

public class ParcelUtils {
	private static final String TAG = ParcelUtils.class.getName();
	private static final String NO_NAME = "!@#$%^&*()";
	private static final String CREATE = "create";

	private static final int VAL_LIST2 = 25;
	private static final int VAL_MAP2 = 26;
	private static final int VAL_MULTIMAP = 27;
	private static final int VAL_OPTIONAL = 28;
	private static final int VAL_RANGE = 29;
	private static final int VAL_RANGEMAP = 30;
	private static final int VAL_SET = 31;
	private static final int VAL_TRIE = 32;

	private ParcelUtils() {}

	// Ewww reflection
	private static boolean isAssignableFrom(Class<?> cls, String asgnName) throws IllegalArgumentException {
		try {
			return cls.isAssignableFrom(Class.forName(asgnName));
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException();
		}
	}

	// This is bad, bad design practice but there's not another to do what I need
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

	// Seriously don't copy this elsewhere
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

	public static <E> List<E> readList(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableList.class, name)) {
			return readImmutableList(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readListInternal(p, name);
		}
	}

	private static <E> List<E> readListInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		List<E> outVal = (List<E>) newInstance(name);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			outVal.add(elem);
		}
		return outVal;
	}

	public static <E> ImmutableList<E> readImmutableList(Parcel p) {
		int n = p.readInt();
		ImmutableList.Builder<E> builder = ImmutableList.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			builder.add(elem);
		}
		return builder.build();
	}

	public static <K, V> Map<K, V> readMap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableMap.class, name)) {
			return readImmutableMap(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readMapInternal(p, name);
		}
	}

	private static <K, V> Map<K, V> readMapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		Map<K, V> outVal = (Map<K, V>) newInstance(name);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			@SuppressWarnings("unchecked")
			V v = (V) readValue(p);
			outVal.put(k, v);
		}
		return outVal;
	}

	public static <K, V> ImmutableMap<K, V> readImmutableMap(Parcel p) {
		int n = p.readInt();
		ImmutableMap.Builder<K, V> builder = ImmutableMap.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			@SuppressWarnings("unchecked")
			V v = (V) readValue(p);
			builder.put(k, v);
		}
		return builder.build();
	}

	public static <K, V> Multimap<K, V> readMultimap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableListMultimap.class, name)) {
			return readImmutableListMultimap(p);
		} else if (isAssignableFrom(ImmutableSetMultimap.class, name)) {
			return readImmutableSetMultimap(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readMultimapInternal(p, name);
		}
	}

	public static <K, V> ListMultimap<K, V> readListMultimap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableListMultimap.class, name)) {
			return readImmutableListMultimap(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readListMultimapInternal(p, name);
		}
	}

	public static <K, V> SetMultimap<K, V> readSetMultimap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableSetMultimap.class, name)) {
			return readImmutableSetMultimap(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readSetMultimapInternal(p, name);
		}
	}

	public static <K, V> SortedSetMultimap<K, V> readSortedSetMultimap(Parcel p) {
		String name = p.readString();
		if (name.equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			SortedSetMultimap<K, V> outVal = (SortedSetMultimap<K, V>) singletonInstance(name, CREATE);

			for (int i = 0; i < n; i++) {
				@SuppressWarnings("unchecked")
				K k = (K) readValue(p);
				int q = p.readInt();
				for (int j = 0; j < q; j++) {
					@SuppressWarnings("unchecked")
					V v = (V) readValue(p);
					outVal.put(k, v);
				}
			}
			return outVal;
		}
	}

	public static <K, V> ImmutableListMultimap<K, V> readImmutableListMultimap(Parcel p) {
		int n = p.readInt();
		ImmutableListMultimap.Builder<K, V> builder = ImmutableListMultimap.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			int q = p.readInt();
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) readValue(p);
				builder.put(k, v);
			}
		}
		return builder.build();
	}

	public static <K, V> ImmutableSetMultimap<K, V> readImmutableSetMultimap(Parcel p) {
		int n = p.readInt();
		ImmutableSetMultimap.Builder<K, V> builder = ImmutableSetMultimap.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			int q = p.readInt();
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) readValue(p);
				builder.put(k, v);
			}
		}
		return builder.build();
	}

	private static <K, V> ListMultimap<K, V> readListMultimapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		ListMultimap<K, V> outVal = (ListMultimap<K, V>) singletonInstance(name, CREATE);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			int q = p.readInt();
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) readValue(p);
				outVal.put(k, v);
			}
		}
		return outVal;
	}

	private static <K, V> SetMultimap<K, V> readSetMultimapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		SetMultimap<K, V> outVal = (SetMultimap<K, V>) singletonInstance(name, CREATE);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			int q = p.readInt();
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) readValue(p);
				outVal.put(k, v);
			}
		}
		return outVal;
	}

	private static <K, V> Multimap<K, V> readMultimapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		Multimap<K, V> outVal = (Multimap<K, V>) singletonInstance(name, CREATE);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			int q = p.readInt();
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) readValue(p);
				outVal.put(k, v);
			}
		}
		return outVal;
	}

	public static <T> Optional<T> readOptional(Parcel p) {
		int s = p.readInt();
		if (s == -1) {
			return null;
		} else if (s == 0) {
			return Optional.absent();
		} else {
			@SuppressWarnings("unchecked")
			T thing = (T) readValue(p);
			return Optional.of(thing);
		}
	}

	public static <C extends Comparable<?>> Range<C> readRange(Parcel p) {
		if (p.readInt() == -1) {
			return null;
		} else {
			int lowerOrd = p.readInt();
			@SuppressWarnings("unchecked")
			C lowerVal = (C) readValue(p);
			int upperOrd = p.readInt();
			@SuppressWarnings("unchecked")
			C upperVal = (C) readValue(p);

			if (lowerVal != null && upperVal != null) {
				BoundType lowerType = BoundType.values()[lowerOrd];
				BoundType upperType = BoundType.values()[upperOrd];
				return Range.range(lowerVal, lowerType, upperVal, upperType);
			} else if (lowerVal != null) {
				BoundType lowerType = BoundType.values()[lowerOrd];
				return Range.downTo(lowerVal, lowerType);
			} else if (upperVal != null) {
				BoundType upperType = BoundType.values()[upperOrd];
				return Range.upTo(upperVal, upperType);
			} else {
				return Range.all();
			}
		}
	}

	public static <K extends Comparable<?>, V> RangeMap<K, V> readRangeMap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableRangeMap.class, name)) {
			return readImmutableRangeMap(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readRangeMapInternal(p, name);
		}
	}

	public static <K extends Comparable<?>, V> ImmutableRangeMap<K, V> readImmutableRangeMap(Parcel p) {
		int n = p.readInt();
		ImmutableRangeMap.Builder<K, V> builder = ImmutableRangeMap.builder();

		for (int i = 0; i < n; i++) {
			Range<K> range = readRange(p);
			@SuppressWarnings("unchecked")
			V value = (V) readValue(p);
			builder.put(range, value);
		}

		return builder.build();
	}

	private static <K extends Comparable<?>, V> RangeMap<K, V> readRangeMapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		RangeMap<K, V> outVal = (RangeMap<K, V>) singletonInstance(name, CREATE);

		for (int i = 0; i < n; i++) {
			Range<K> range = readRange(p);
			@SuppressWarnings("unchecked")
			V value = (V) readValue(p);
			outVal.put(range, value);
		}

		return outVal;
	}

	public static <E> Set<E> readSet(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableSet.class, name)) {
			return readImmutableSet(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readSetInternal(p, name);
		}
	}

	public static <E> ImmutableSet<E> readImmutableSet(Parcel p) {
		int n = p.readInt();
		ImmutableSet.Builder<E> builder = ImmutableSet.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			builder.add(elem);
		}
		return builder.build();
	}

	private static <E> Set<E> readSetInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		Set<E> outVal = (Set<E>) newInstance(name);

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			outVal.add(elem);
		}
		return outVal;
	}

	public static <K, V> Trie<K, V> readTrie(Parcel p) {
		String name = p.readString();
		if (name.equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			Trie<K, V> outVal = (Trie<K, V>) newInstance(name);

			for (int i = 0; i < n; i++) {
				@SuppressWarnings("unchecked")
				K k = (K) readValue(p);
				@SuppressWarnings("unchecked")
				V v = (V) readValue(p);
				outVal.put(k, v);
			}
			return outVal;
		}
	}

	public static final Object readValue(Parcel p) {
		int type = p.readInt();

		switch (type) {
		case VAL_LIST2:
			return readList(p);
		case VAL_MAP2:
			return readMap(p);
		case VAL_MULTIMAP:
			return readMultimap(p);
		case VAL_OPTIONAL:
			return readOptional(p);
		case VAL_RANGE:
			return readRange(p);
		case VAL_RANGEMAP:
			return readRangeMap(p);
		case VAL_SET:
			return readSet(p);
		case VAL_TRIE:
			return readTrie(p);
		default:
			/* For some bizarre reason, you have to pass in a ClassLoader from a class
			 * in the app itself. Using a ClassLoader from a regular Java class won't
			 * work. I have absolutely no idea why.
			 */
			return p.readValue(ParcelUtils.class.getClassLoader());
		}
	}

	public static <E> void writeList(Parcel p, List<E> list) {
		if (list == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(list.getClass().getCanonicalName());
			p.writeInt(list.size());
			for (E elem : list) {
				writeValue(p, elem);
			}
		}
	}

	public static <K, V> void writeMap(Parcel p, Map<K, V> map) {
		if (map == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(map.getClass().getCanonicalName());
			p.writeInt(map.size());
			for (Map.Entry<K, V> entry : map.entrySet()) {
				writeValue(p, entry.getKey());
				writeValue(p, entry.getValue());
			}
		}
	}

	public static <K, V> void writeMultimap(Parcel p, Multimap<K, V> multimap) {
		if (multimap == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(multimap.getClass().getCanonicalName());
			p.writeInt(multimap.size());
			for (Map.Entry<K, Collection<V>> entry : multimap.asMap().entrySet()) {
				writeValue(p, entry.getKey());
				p.writeInt(entry.getValue().size());
				for (V v : entry.getValue()) {
					writeValue(p, v);
				}
			}
		}
	}

	public static <T> void writeOptional(Parcel p, Optional<T> opt) {
		if (opt == null) {
			p.writeInt(-1);
		} else if (opt.isPresent()) {
			p.writeInt(1);
			writeValue(p, opt.get());
		} else {
			p.writeInt(0);
		}
	}

	public static <C extends Comparable<?>> void writeRange(Parcel p, Range<C> range) {
		if (range == null) {
			p.writeInt(-1);
		} else {
			p.writeInt(1);

			if (range.hasLowerBound()) {
				p.writeInt(range.lowerBoundType().ordinal());
				writeValue(p, range.lowerEndpoint());
			} else {
				p.writeInt(-1);
				p.writeValue(null);
			}

			if (range.hasUpperBound()) {
				p.writeInt(range.upperBoundType().ordinal());
				writeValue(p, range.upperEndpoint());
			} else {
				p.writeInt(-1);
				p.writeValue(null);
			}
		}
	}

	public static <K extends Comparable<?>, V> void writeRangeMap(Parcel p, RangeMap<K, V> rangeMap) {
		if (rangeMap == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(rangeMap.getClass().getCanonicalName());
			p.writeInt(rangeMap.asMapOfRanges().size());
			for (Map.Entry<Range<K>, V> entry : rangeMap.asMapOfRanges().entrySet()) {
				writeRange(p, entry.getKey());
				writeValue(p, entry.getValue());
			}
		}
	}

	public static <E> void writeSet(Parcel p, Set<E> set) {
		if (set == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(set.getClass().getCanonicalName());
			p.writeInt(set.size());
			for (E elem : set) {
				writeValue(p, elem);
			}
		}
	}

	public static <K, V> void writeTrie(Parcel p, Trie<K, V> trie) {
		if (trie == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(trie.getClass().getCanonicalName());
			p.writeInt(trie.size());
			for (Map.Entry<K, V> entry : trie.entrySet()) {
				writeValue(p, entry.getKey());
				writeValue(p, entry.getValue());
			}
		}
	}

	public static void writeValue(Parcel p, Object v) {
		if (v instanceof List) {
			p.writeInt(VAL_LIST2);
			writeList(p, (List<?>) v);
		} else if (v instanceof Map) {
			p.writeInt(VAL_MAP2);
			writeMap(p, (Map<?, ?>) v);
		} else if (v instanceof Multimap) {
			p.writeInt(VAL_MULTIMAP);
			writeMultimap(p, (Multimap<?, ?>) v);
		} else if (v instanceof Optional) {
			p.writeInt(VAL_OPTIONAL);
			writeOptional(p, (Optional<?>) v);
		} else if (v instanceof Range) {
			p.writeInt(VAL_RANGE);
			@SuppressWarnings("unchecked")
			Range<? extends Comparable<?>> range = (Range<? extends Comparable<?>>) v;
			writeRange(p, range);
		} else if (v instanceof RangeMap) {
			p.writeInt(VAL_RANGEMAP);
			@SuppressWarnings("unchecked")
			RangeMap<? extends Comparable<?>, ?> rangeMap = (RangeMap<? extends Comparable<?>, ?>) v;
			writeRangeMap(p, rangeMap);
		} else if (v instanceof Set) {
			p.writeInt(VAL_SET);
			writeSet(p, (Set<?>) v);
		} else if (v instanceof Trie) {
			p.writeInt(VAL_TRIE);
			writeTrie(p, (Trie<?, ?>) v);
		} else { // Delegate to default writeValue()
			p.writeInt(-2);
			p.writeValue(v);
		}
	}

}
