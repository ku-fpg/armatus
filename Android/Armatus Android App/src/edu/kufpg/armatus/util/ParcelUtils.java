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
	private static final String TAG = ParcelUtils.class.getSimpleName();
	private static final String NO_NAME = "!@#$%^&*()";
	private static final String CREATE = "create";

	private static final int VAL_BOOLEAN = 25;
	private static final int VAL_ENUM = 26;
	private static final int VAL_LIST2 = 27;
	private static final int VAL_MAP2 = 28;
	private static final int VAL_MULTIMAP = 29;
	private static final int VAL_OPTIONAL = 30;
	private static final int VAL_RANGE = 31;
	private static final int VAL_RANGEMAP = 32;
	private static final int VAL_SET = 33;
	private static final int VAL_TRIE = 34;

	private ParcelUtils() {}
	
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
	
	// Ewww reflection
	private static boolean isAssignableFrom(Class<?> cls, String asgnName) throws IllegalArgumentException {
		try {
			return cls.isAssignableFrom(Class.forName(asgnName));
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException();
		}
	}

	// This is bad, bad design practice but there's not another way to do what I need
	private static Object newInstance(String className) throws IllegalArgumentException {
		try {
			Class<?> c = Class.forName(className);
			return c.newInstance();
		} catch (ClassNotFoundException e) {
			Log.e(TAG, "Illegal access when unmarshalling: "
					+ className, e);
			throw new IllegalArgumentException("ClassNotFoundException when unmarshalling: "
					+ className);
		} catch (InstantiationException e) {
			Log.e(TAG, "Class not found when unmarshalling: "
					+ className, e);
			throw new IllegalArgumentException("InstantiationException when unmarshalling: "
					+ className);
		} catch (IllegalAccessException e) {
			throw new IllegalArgumentException("IllegalAccessException when unmarshalling: "
					+ className);
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
	
	public static boolean readBoolean(Parcel p) {
		return p.readInt() == 1 ? true : false;
	}
	
	public static <E extends Enum<E>> E readEnum(Parcel p) {
		String className = p.readString();
		if (className.equals(NO_NAME)) {
			return null;
		} else {
			@SuppressWarnings("unchecked")
			Class<E> cls = (Class<E>) forName(className);
			String valueName = p.readString();
			return Enum.valueOf(cls, valueName);
		}
	}

	public static <E> List<E> readList(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableList.class, name)) {
			return readImmutableListInternal(p);
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
		p.readString();
		int n = p.readInt();
		ImmutableList.Builder<E> builder = ImmutableList.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			builder.add(elem);
		}
		return builder.build();
	}
	
	public static <E> ImmutableList<E> readImmutableListInternal(Parcel p) {
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
			return readImmutableMapInternal(p);
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
		p.readString();
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
	
	public static <K, V> ImmutableMap<K, V> readImmutableMapInternal(Parcel p) {
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
			return readImmutableListMultimapInternal(p);
		} else if (isAssignableFrom(ImmutableSetMultimap.class, name)) {
			return readImmutableSetMultimapInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readMultimapInternal(p, name);
		}
	}

	public static <K, V> ListMultimap<K, V> readListMultimap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableListMultimap.class, name)) {
			return readImmutableListMultimapInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readListMultimapInternal(p, name);
		}
	}

	public static <K, V> SetMultimap<K, V> readSetMultimap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableSetMultimap.class, name)) {
			return readImmutableSetMultimapInternal(p);
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
		p.readString();
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
	
	public static <K, V> ImmutableListMultimap<K, V> readImmutableListMultimapInternal(Parcel p) {
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
		p.readString();
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
	
	public static <K, V> ImmutableSetMultimap<K, V> readImmutableSetMultimapInternal(Parcel p) {
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

	public static <C extends Comparable<? super C>> Range<C> readRange(Parcel p) {
		if (p.readInt() == -1) {
			return null;
		} else {
			BoundType lowerType = readEnum(p);
			@SuppressWarnings("unchecked")
			C lowerVal = (C) readValue(p);
			
			BoundType upperType = readEnum(p);
			@SuppressWarnings("unchecked")
			C upperVal = (C) readValue(p);

			if (lowerVal != null && upperVal != null) {
				return Range.range(lowerVal, lowerType, upperVal, upperType);
			} else if (lowerVal != null) {
				return Range.downTo(lowerVal, lowerType);
			} else if (upperVal != null) {
				return Range.upTo(upperVal, upperType);
			} else {
				return Range.all();
			}
		}
	}

	public static <K extends Comparable<? super K>, V> RangeMap<K, V> readRangeMap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableRangeMap.class, name)) {
			return readImmutableRangeMapInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readRangeMapInternal(p, name);
		}
	}

	public static <K extends Comparable<? super K>, V> ImmutableRangeMap<K, V> readImmutableRangeMap(Parcel p) {
		p.readString();
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
	
	public static <K extends Comparable<? super K>, V> ImmutableRangeMap<K, V> readImmutableRangeMapInternal(Parcel p) {
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

	private static <K extends Comparable<? super K>, V> RangeMap<K, V> readRangeMapInternal(Parcel p, String name) {
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
			return readImmutableSetInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readSetInternal(p, name);
		}
	}

	public static <E> ImmutableSet<E> readImmutableSet(Parcel p) {
		p.readString();
		int n = p.readInt();
		ImmutableSet.Builder<E> builder = ImmutableSet.builder();

		for (int i = 0; i < n; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			builder.add(elem);
		}
		return builder.build();
	}
	
	public static <E> ImmutableSet<E> readImmutableSetInternal(Parcel p) {
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
		case VAL_BOOLEAN:
			return readBoolean(p);
		case VAL_ENUM:
			return readEnum(p);
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
	
	public static void writeBoolean(Parcel p, boolean b) {
		p.writeInt(b ? 1 : 0);
	}
	
	public static <E extends Enum<?>> void writeEnum(Parcel p, E enumVal) {
		if (enumVal == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(enumVal.getDeclaringClass().getName());
			p.writeString(enumVal.name());
		}
	}

	public static <E> void writeList(Parcel p, List<E> list) {
		if (list == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(list.getClass().getName());
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
			p.writeString(map.getClass().getName());
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
			p.writeString(multimap.getClass().getName());
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
				writeEnum(p, range.lowerBoundType());
				writeValue(p, range.lowerEndpoint());
			} else {
				writeEnum(p, null);
				p.writeValue(null);
			}

			if (range.hasUpperBound()) {
				writeEnum(p, range.upperBoundType());
				writeValue(p, range.upperEndpoint());
			} else {
				writeEnum(p, null);
				p.writeValue(null);
			}
		}
	}

	public static <K extends Comparable<?>, V> void writeRangeMap(Parcel p, RangeMap<K, V> rangeMap) {
		if (rangeMap == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(rangeMap.getClass().getName());
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
			p.writeString(set.getClass().getName());
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
			p.writeString(trie.getClass().getName());
			p.writeInt(trie.size());
			for (Map.Entry<K, V> entry : trie.entrySet()) {
				writeValue(p, entry.getKey());
				writeValue(p, entry.getValue());
			}
		}
	}

	public static void writeValue(Parcel p, Object v) {
		if (v instanceof Boolean) {
			p.writeInt(VAL_BOOLEAN);
			writeBoolean(p, (Boolean) v);
		} else if (v.getClass().isEnum()) {
			p.writeInt(VAL_BOOLEAN);
			writeEnum(p, (Enum<?>) v);
		} else if (v instanceof List) {
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
