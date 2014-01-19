package edu.kufpg.armatus.util;

import java.io.Serializable;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.Deque;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NavigableMap;
import java.util.NavigableSet;
import java.util.PriorityQueue;
import java.util.Queue;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import org.apache.commons.collections4.trie.PatriciaTrie;

import android.os.Parcel;
import android.os.Parcelable;
import android.util.Log;

import com.google.common.base.Optional;
import com.google.common.collect.ArrayListMultimap;
import com.google.common.collect.BiMap;
import com.google.common.collect.BoundType;
import com.google.common.collect.HashBiMap;
import com.google.common.collect.HashMultimap;
import com.google.common.collect.HashMultiset;
import com.google.common.collect.ImmutableBiMap;
import com.google.common.collect.ImmutableCollection;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.ImmutableListMultimap;
import com.google.common.collect.ImmutableMap;
import com.google.common.collect.ImmutableMultimap;
import com.google.common.collect.ImmutableMultiset;
import com.google.common.collect.ImmutableRangeMap;
import com.google.common.collect.ImmutableSet;
import com.google.common.collect.ImmutableSetMultimap;
import com.google.common.collect.ImmutableSortedMap;
import com.google.common.collect.ImmutableSortedMultiset;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.LinkedHashMultiset;
import com.google.common.collect.LinkedListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;
import com.google.common.collect.Ordering;
import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.SortedMultiset;
import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;
import com.google.common.collect.TreeMultiset;
import com.google.common.collect.TreeRangeMap;

public class ParcelUtils {
	private static final Class<?>[] COMPARATOR_ARG = new Class<?>[] { Comparator.class };
	private static final String TAG = ParcelUtils.class.getSimpleName();
	private static final String CREATE = "create";
	private static final String BUILDER = "builder";

	private static final int VAL_BOOLEAN = 25;
	private static final int VAL_ENUM = 26;
	private static final int VAL_LIST2 = 27;
	private static final int VAL_PATRICIATRIE = 28;
	private static final int VAL_SORTEDMAP = 29;
	private static final int VAL_MAP2 = 30;
	private static final int VAL_TREEMULTIMAP = 31;
	private static final int VAL_SORTEDSETMULTIMAP = 32;
	private static final int VAL_MULTIMAP = 33;
	private static final int VAL_SORTEDMULTISET = 34;
	private static final int VAL_MULTISET = 35;
	private static final int VAL_OPTIONAL = 36;
	private static final int VAL_PRIORITYQUEUE = 37;
	private static final int VAL_QUEUE = 38;
	private static final int VAL_RANGE = 39;
	private static final int VAL_RANGEMAP = 40;
	private static final int VAL_SORTEDSET = 41;
	private static final int VAL_SET = 42;
	private static final int VAL_COLLECTION = 43;

	private ParcelUtils() {}

	// Warning: extremely bad code
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

	// Eww reflection
	private static boolean isAssignableFrom(Class<?> cls, String asgnName) throws IllegalArgumentException {
		try {
			return cls.isAssignableFrom(Class.forName(asgnName));
		} catch (ClassNotFoundException e) {
			throw new IllegalArgumentException();
		}
	}

	// It's so bad
	private static Object newInstance(String className) throws IllegalArgumentException {
		return newInstance(className, (Class<?>[]) null, (Object[]) null);
	}

	// This is bad, bad design practice but there's not another way to do what I need
	private static Object newInstance(String className, Class<?>[] parameterTypes, Object... constructorArgs) throws IllegalArgumentException {
		try {
			Class<?> cls = Class.forName(className);
			if (constructorArgs == null || constructorArgs.length == 0) {
				return cls.newInstance();
			} else {
				Constructor<?> constr = cls.getConstructor(parameterTypes);
				return constr.newInstance(constructorArgs);
			}
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
		} catch (NoSuchMethodException e) {
			throw new IllegalArgumentException("NoSuchMethodException when unmarshalling: "
					+ className);
		} catch (InvocationTargetException e) {
			throw new IllegalArgumentException("InvocationTargetException when unmarshalling: "
					+ className);
		}
	}

	// I can't believe I actually wrote this code
	private static Object singletonInstance(String className, String singletonMethodName) throws IllegalArgumentException {
		return singletonInstance(className, singletonMethodName, (Object[]) null);
	}

	// Seriously don't copy this elsewhere
	private static Object singletonInstance(String className, String singletonMethodName, Object... methodArgs) throws IllegalArgumentException {
		try {
			Class<?> c = Class.forName(className);
			Method m = c.getMethod(singletonMethodName);
			return m.invoke(null, methodArgs);
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
		return p.readInt() != 0;
	}

	public static <E extends Enum<E>> E readEnum(Parcel p) {
		String className = p.readString();

		if (className == null) {
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

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableList.class, name)) {
			return readImmutableListInternal(p);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			List<E> outVal = (List<E>) newInstance(name);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> ArrayList<E> readArrayList(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			ArrayList<E> outVal = new ArrayList<E>(n);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> LinkedList<E> readLinkedList(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			LinkedList<E> outVal = new LinkedList<E>();
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> Stack<E> readStack(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			Stack<E> outVal = new Stack<E>();
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> Vector<E> readVector(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			Vector<E> outVal = new Vector<E>(n);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> ImmutableList<E> readImmutableList(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readImmutableListInternal(p);
		}
	}

	private static <E> ImmutableList<E> readImmutableListInternal(Parcel p) {
		int n = p.readInt();
		ImmutableList.Builder<E> builder = ImmutableList.builder();
		addToImmutableCollectionBuilder(p, builder, n);
		return builder.build();
	}

	public static <K, V> Map<K, V> readMap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMap.class, name)) {
			return readImmutableSortedMapInternal(p);
		} else if (isAssignableFrom(ImmutableBiMap.class, name)) {
			return readImmutableBiMapInternal(p);
		} else if (isAssignableFrom(ImmutableMap.class, name)) {
			return readImmutableMapInternal(p);
		} else if (isAssignableFrom(SortedMap.class, name)) {
			return readSortedMapInternal(p, name);
		} else if (isAssignableFrom(BiMap.class, name)) {
			return readBiMapInternal(p, name);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			Map<K, V> outVal = (Map<K, V>) newInstance(name);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> SortedMap<K, V> readSortedMap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMap.class, name)) {
			return readImmutableSortedMapInternal(p);
		} else if (isAssignableFrom(PatriciaTrie.class, name)) {
			@SuppressWarnings("unchecked")
			SortedMap<K, V> sortedMap = (SortedMap<K, V>) readPatriciaTrieInternal(p);
			return sortedMap;
		} else {
			return readSortedMapInternal(p, name);
		}
	}

	private static <K, V> SortedMap<K, V> readSortedMapInternal(Parcel p, String name) {
		int n = p.readInt();
		Comparator<? super K> c = readComparator(p);
		if (c == null) {
			@SuppressWarnings("unchecked")
			SortedMap<K, V> outVal = (SortedMap<K, V>) newInstance(name);
			return addToMap(p, outVal, n);
		} else {
			@SuppressWarnings("unchecked")
			SortedMap<K, V> outVal = (SortedMap<K, V>) newInstance(name, COMPARATOR_ARG, c);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> NavigableMap<K, V> readNavigableMap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMap.class, name)) {
			return readImmutableSortedMapInternal(p);
		} else {
			int n = p.readInt();
			Comparator<? super K> c = readComparator(p);
			if (c == null) {
				@SuppressWarnings("unchecked")
				NavigableMap<K, V> outVal = (NavigableMap<K, V>) newInstance(name);
				return addToMap(p, outVal, n);
			} else {
				@SuppressWarnings("unchecked")
				NavigableMap<K, V> outVal = (NavigableMap<K, V>) newInstance(name, COMPARATOR_ARG, c);
				return addToMap(p, outVal, n);
			}
		}
	}

	public static <K, V> BiMap<K, V> readBiMap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableBiMap.class, name)) {
			return readImmutableBiMapInternal(p);
		} else {
			return readBiMapInternal(p, name);
		}
	}

	private static <K, V> BiMap<K, V> readBiMapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		BiMap<K, V> outVal = (BiMap<K, V>) singletonInstance(name, CREATE);
		return addToMap(p, outVal, n);
	}

	//	public static <K extends Enum<K>, V extends Enum<V>> EnumBiMap<K, V> readEnumBiMap(Parcel p) {
	//		if (p.readString().equals(NO_NAME)) {
	//			return null;
	//		} else {
	//			int n = p.readInt();
	//			EnumBiMap<K, V> outVal = EnumBiMap.create(???, ???);
	//
	//			for (int i = 0; i < n; i++) {
	//				@SuppressWarnings("unchecked")
	//				K k = (K) readValue(p);
	//				@SuppressWarnings("unchecked")
	//				V v = (V) readValue(p);
	//				outVal.put(k, v);
	//			}
	//			return outVal;
	//		}
	//	}
	//
	//	public static <K extends Enum<K>, V> EnumHashBiMap<K, V> readEnumBiMap(Parcel p) {
	//		if (p.readString().equals(NO_NAME)) {
	//			return null;
	//		} else {
	//			int n = p.readInt();
	//			EnumHashBiMap<K, V> outVal = EnumHashBiMap.create(???);
	//
	//			for (int i = 0; i < n; i++) {
	//				@SuppressWarnings("unchecked")
	//				K k = (K) readValue(p);
	//				@SuppressWarnings("unchecked")
	//				V v = (V) readValue(p);
	//				outVal.put(k, v);
	//			}
	//			return outVal;
	//		}
	//	}
	//	
	//	public static <K extends Enum<K>, V> EnumMap<K, V> readEnumMap(Parcel p) {
	//		if (p.readString().equals(NO_NAME)) {
	//			return null;
	//		} else {
	//			int n = p.readInt();
	//			EnumMap<K, V> outVal = new EnumMap<K, V>();
	//
	//			for (int i = 0; i < n; i++) {
	//				@SuppressWarnings("unchecked")
	//				K k = (K) readValue(p);
	//				@SuppressWarnings("unchecked")
	//				V v = (V) readValue(p);
	//				outVal.put(k, v);
	//			}
	//			return outVal;
	//		}
	//	}

	public static <K, V> HashBiMap<K, V> readHashBiMap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			HashBiMap<K, V> outVal = HashBiMap.create(n);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> HashMap<K, V> readHashMap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			HashMap<K, V> outVal = new HashMap<K, V>(n);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> Hashtable<K, V> readHashtable(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			Hashtable<K, V> outVal = new Hashtable<K, V>(n);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> LinkedHashMap<K, V> readLinkedHashMap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			LinkedHashMap<K, V> outVal = new LinkedHashMap<K, V>(n);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> TreeMap<K, V> readTreeMap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			Comparator<? super K> c = readComparator(p);
			TreeMap<K, V> outVal;
			if (c == null) {
				outVal = new TreeMap<K, V>();
			} else {
				outVal = new TreeMap<K, V>(c);
			}
			return addToMap(p, outVal, n);
		}
	}

	private static <K, V, M extends Map<K, V>> M addToMap(Parcel p, M outVal, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			@SuppressWarnings("unchecked")
			V v = (V) readValue(p);
			outVal.put(k, v);
		}
		return outVal;
	}

	public static <K, V> ImmutableMap<K, V> readImmutableMap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMap.class, name)) {
			return readImmutableSortedMapInternal(p);
		} else {
			return readImmutableMapInternal(p);
		}
	}

	private static <K, V> ImmutableMap<K, V> readImmutableMapInternal(Parcel p) {
		int n = p.readInt();
		ImmutableMap.Builder<K, V> builder = ImmutableMap.builder();
		addToImmutableMapBuilder(p, builder, n);
		return builder.build();
	}

	public static <K, V> ImmutableSortedMap<K, V> readImmutableSortedMap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readImmutableSortedMapInternal(p);
		}
	}

	private static <K, V> ImmutableSortedMap<K, V> readImmutableSortedMapInternal(Parcel p) {
		int n = p.readInt();
		Comparator<K> c = readComparator(p);
		if (c == null) {
			@SuppressWarnings("unchecked")
			ImmutableSortedMap.Builder<K, V> builder = (ImmutableSortedMap.Builder<K, V>) ImmutableSortedMap.naturalOrder();
			addToImmutableMapBuilder(p, builder, n);
			return builder.build();
		} else {
			ImmutableSortedMap.Builder<K, V> builder = ImmutableSortedMap.orderedBy(c);
			addToImmutableMapBuilder(p, builder, n);
			return builder.build();
		}
	}

	public static <K, V> ImmutableBiMap<K, V> readImmutableBiMap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readImmutableBiMapInternal(p);
		}
	}

	private static <K, V> ImmutableBiMap<K, V> readImmutableBiMapInternal(Parcel p) {
		int n = p.readInt();
		ImmutableBiMap.Builder<K, V> builder = ImmutableBiMap.builder();
		addToImmutableMapBuilder(p, builder, n);
		return builder.build();
	}

	private static <K, V> void addToImmutableMapBuilder(Parcel p, ImmutableMap.Builder<K, V> builder, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			@SuppressWarnings("unchecked")
			V v = (V) readValue(p);
			builder.put(k, v);
		}
	}

	public static <K, V> Multimap<K, V> readMultimap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableListMultimap.class, name)) {
			return readImmutableListMultimapInternal(p);
		} else if (isAssignableFrom(ImmutableSetMultimap.class, name)) {
			return readImmutableSetMultimapInternal(p);
		} else if (isAssignableFrom(TreeMultimap.class, name)) {
			return readTreeMultimapInternal(p);
		} else if (isAssignableFrom(SortedSetMultimap.class, name)) {
			return readSortedSetMultimapInternal(p, name);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			Multimap<K, V> outVal = (Multimap<K, V>) singletonInstance(name, CREATE);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> ListMultimap<K, V> readListMultimap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableListMultimap.class, name)) {
			return readImmutableListMultimapInternal(p);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			ListMultimap<K, V> outVal = (ListMultimap<K, V>) singletonInstance(name, CREATE);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> ArrayListMultimap<K, V> readArrayListMultimap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			ArrayListMultimap<K, V> outVal = ArrayListMultimap.create(n, 3);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> LinkedListMultimap<K, V> readLinkedListMultimap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			LinkedListMultimap<K, V> outVal = LinkedListMultimap.create(n);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> SetMultimap<K, V> readSetMultimap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSetMultimap.class, name)) {
			return readImmutableSetMultimapInternal(p);
		} else if (isAssignableFrom(TreeMultimap.class, name)) {
			return readTreeMultimapInternal(p);
		} else if (isAssignableFrom(SortedSetMultimap.class, name)) {
			return readSortedSetMultimapInternal(p, name);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			SetMultimap<K, V> outVal = (SetMultimap<K, V>) singletonInstance(name, CREATE);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> HashMultimap<K, V> readHashMultimap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			HashMultimap<K, V> outVal = HashMultimap.create(n, 2);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> LinkedHashMultimap<K, V> readLinkedHashMultimap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			LinkedHashMultimap<K, V> outVal = LinkedHashMultimap.create(n, 2);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> SortedSetMultimap<K, V> readSortedSetMultimap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(TreeMultimap.class, name)) {
			return readTreeMultimapInternal(p);
		} else {
			return readSortedSetMultimapInternal(p, name);
		}
	}

	private static <K, V> SortedSetMultimap<K, V> readSortedSetMultimapInternal(Parcel p, String name) {
		int n = p.readInt();
		Comparator<? super V> valC = readComparator(p);
		if (valC == null) {
			@SuppressWarnings("unchecked")
			SortedSetMultimap<K, V> outVal = (SortedSetMultimap<K, V>) singletonInstance(name, CREATE);
			return addToMultimap(p, outVal, n);
		} else {
			@SuppressWarnings("unchecked")
			SortedSetMultimap<K, V> outVal = (SortedSetMultimap<K, V>) singletonInstance(name, CREATE, valC);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> TreeMultimap<K, V> readTreeMultimap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readTreeMultimapInternal(p);
		}
	}

	private static <K, V> TreeMultimap<K, V> readTreeMultimapInternal(Parcel p) {
		int n = p.readInt();
		Comparator<? super K> keyC = readComparator(p);
		Comparator<? super V> valC = readComparator(p);
		if (keyC == null && valC == null) {
			@SuppressWarnings("unchecked")
			TreeMultimap<K, V> outVal = (TreeMultimap<K, V>) TreeMultimap.create();
			return addToMultimap(p, outVal, n);
		} else if (keyC == null && valC != null) {
			@SuppressWarnings("unchecked")
			TreeMultimap<K, V> outVal = (TreeMultimap<K, V>) TreeMultimap.create(Ordering.natural(), valC);
			return addToMultimap(p, outVal, n);
		} else if (keyC != null && valC == null) {
			@SuppressWarnings("unchecked")
			TreeMultimap<K, V> outVal = (TreeMultimap<K, V>) TreeMultimap.create(keyC, Ordering.natural());
			return addToMultimap(p, outVal, n);
		} else {
			TreeMultimap<K, V> outVal = TreeMultimap.create(keyC, valC);
			return addToMultimap(p, outVal, n);
		}
	}

	private static <K, V, M extends Multimap<K, V>> M addToMultimap(Parcel p, M outVal, int size) {
		for (int i = 0; i < size; i++) {
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

	public static <K, V> ImmutableListMultimap<K, V> readImmutableListMultimap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readImmutableListMultimapInternal(p);
		}
	}

	private static <K, V> ImmutableListMultimap<K, V> readImmutableListMultimapInternal(Parcel p) {
		int n = p.readInt();
		ImmutableListMultimap.Builder<K, V> builder = ImmutableListMultimap.builder();
		addToImmutableMultimapBuilder(p, builder, n);
		return builder.build();
	}

	public static <K, V> ImmutableSetMultimap<K, V> readImmutableSetMultimap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readImmutableSetMultimapInternal(p);
		}
	}

	private static <K, V> ImmutableSetMultimap<K, V> readImmutableSetMultimapInternal(Parcel p) {
		int n = p.readInt();
		ImmutableSetMultimap.Builder<K, V> builder = ImmutableSetMultimap.builder();
		addToImmutableMultimapBuilder(p, builder, n);
		return builder.build();
	}

	private static <K, V> void addToImmutableMultimapBuilder(Parcel p, ImmutableMultimap.Builder<K, V> builder, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			K k = (K) readValue(p);
			int q = p.readInt();
			for (int j = 0; j < q; j++) {
				@SuppressWarnings("unchecked")
				V v = (V) readValue(p);
				builder.put(k, v);
			}
		}
	}

	public static <E> Multiset<E> readMultiset(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMultiset.class, name)) {
			return readImmutableSortedMultisetInternal(p);
		} else if (isAssignableFrom(ImmutableMultiset.class, name)) {
			return readImmutableMultisetInternal(p);
		} else if (isAssignableFrom(SortedMultiset.class, name)) {
			return readSortedMultisetInternal(p, name);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			Multiset<E> outVal = (Multiset<E>) singletonInstance(name, CREATE);
			return addToCollection(p, outVal, n);
		}
	}

	//	public static <E extends Enum<E>> EnumMultiset<E> readEnumMultiset(Parcel p) {
	//		if (p.readString().equals(NO_NAME)) {
	//			return null;
	//		} else {
	//			int n = p.readInt();
	//			EnumMultiset<E> outVal = EnumMultiset.create(???);
	//			return addToCollection(p, outVal, n);
	//		}
	//	}

	public static <E> HashMultiset<E> readHashMultiset(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			HashMultiset<E> outVal = HashMultiset.create(n);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> LinkedHashMultiset<E> readLinkedHashMultiset(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			LinkedHashMultiset<E> outVal = LinkedHashMultiset.create(n);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> SortedMultiset<E> readSortedMultiset(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMultiset.class, name)) {
			return readImmutableSortedMultisetInternal(p);
		} else {
			return readSortedMultisetInternal(p, name);
		}
	}

	private static <E> SortedMultiset<E> readSortedMultisetInternal(Parcel p, String name) {
		int n = p.readInt();
		Comparator<? super E> c = readComparator(p);
		if (c == null) {
			@SuppressWarnings("unchecked")
			SortedMultiset<E> outVal = (SortedMultiset<E>) singletonInstance(name, CREATE);
			return addToCollection(p, outVal, n);
		} else {
			@SuppressWarnings("unchecked")
			SortedMultiset<E> outVal = (SortedMultiset<E>) singletonInstance(name, CREATE, c);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> TreeMultiset<E> readTreeMultiset(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			Comparator<? super E> c = readComparator(p);
			if (c == null) {
				@SuppressWarnings("unchecked")
				TreeMultiset<E> outVal = (TreeMultiset<E>) TreeMultiset.create();
				return addToCollection(p, outVal, n);
			} else {
				TreeMultiset<E> outVal = TreeMultiset.create(c);
				return addToCollection(p, outVal, n);
			}
		}
	}

	public static <E> ImmutableMultiset<E> readImmutableMultiset(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMultiset.class, name)) {
			return readImmutableSortedMultisetInternal(p);
		} else {
			return readImmutableMultisetInternal(p);
		}
	}

	private static <E> ImmutableMultiset<E> readImmutableMultisetInternal(Parcel p) {
		int n = p.readInt();
		ImmutableMultiset.Builder<E> builder = ImmutableMultiset.builder();
		addToImmutableCollectionBuilder(p, builder, n);
		return builder.build();
	}

	public static <E> ImmutableSortedMultiset<E> readImmutableSortedMultiset(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readImmutableSortedMultisetInternal(p);
		}
	}

	private static <E> ImmutableSortedMultiset<E> readImmutableSortedMultisetInternal(Parcel p) {
		int n = p.readInt();
		Comparator<E> c = readComparator(p);
		if (c == null) {
			@SuppressWarnings("unchecked")
			ImmutableSortedMultiset.Builder<E> builder = (ImmutableSortedMultiset.Builder<E>) ImmutableSortedMultiset.naturalOrder();
			addToImmutableCollectionBuilder(p, builder, n);
			return builder.build();
		} else {
			ImmutableSortedMultiset.Builder<E> builder = ImmutableSortedMultiset.orderedBy(c);
			addToImmutableCollectionBuilder(p, builder, n);
			return builder.build();
		}
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

	public static <E> Queue<E> readQueue(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(PriorityQueue.class, name)) {
			return readPriorityQueueInternal(p);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			Queue<E> outVal = (Queue<E>) newInstance(name);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> Deque<E> readDeque(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			Deque<E> outVal = (Deque<E>) newInstance(name);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> ArrayDeque<E> readArrayDeque(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			ArrayDeque<E> outVal = new ArrayDeque<E>(n);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> PriorityQueue<E> readPriorityQueue(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readPriorityQueueInternal(p);
		}
	}

	private static <E> PriorityQueue<E> readPriorityQueueInternal(Parcel p) {
		int n = p.readInt();
		Comparator<? super E> c = readComparator(p);
		PriorityQueue<E> outVal;
		if (c == null) {
			outVal = new PriorityQueue<E>(n);
		} else {
			outVal = new PriorityQueue<E>(n, c);
		}
		return addToCollection(p, outVal, n);
	}

	public static <C extends Comparable<?>> Range<C> readRange(Parcel p) {
		int s = p.readInt();

		if (s == -1) {
			return null;
		} else if (s == 0) {
			return Range.all();
		} else if (s == 1) {
			BoundType lowerType = readBoolean(p) ? BoundType.CLOSED : BoundType.OPEN;
			@SuppressWarnings("unchecked")
			C lowerVal = (C) readValue(p);
			return Range.downTo(lowerVal, lowerType);
		} else if (s == 2) {
			BoundType upperType = readBoolean(p) ? BoundType.CLOSED : BoundType.OPEN;
			@SuppressWarnings("unchecked")
			C upperVal = (C) readValue(p);
			return Range.upTo(upperVal, upperType);
		} else {
			BoundType lowerType = readBoolean(p) ? BoundType.CLOSED : BoundType.OPEN;
			@SuppressWarnings("unchecked")
			C lowerVal = (C) readValue(p);

			BoundType upperType = readBoolean(p) ? BoundType.CLOSED : BoundType.OPEN;
			@SuppressWarnings("unchecked")
			C upperVal = (C) readValue(p);
			return Range.range(lowerVal, lowerType, upperVal, upperType);
		}

		//		} else {
		//			BoundType lowerType = readEnum(p);
		//			@SuppressWarnings("unchecked")
		//			C lowerVal = (C) readValue(p);
		//
		//			BoundType upperType = readEnum(p);
		//			@SuppressWarnings("unchecked")
		//			C upperVal = (C) readValue(p);
		//
		//			if (lowerVal != null && upperVal != null) {
		//				return Range.range(lowerVal, lowerType, upperVal, upperType);
		//			} else if (lowerVal != null) {
		//				return Range.downTo(lowerVal, lowerType);
		//			} else if (upperVal != null) {
		//				return Range.upTo(upperVal, upperType);
		//			} else {
		//				return Range.all();
		//			}
		//		}
	}

	public static <K extends Comparable<?>, V> RangeMap<K, V> readRangeMap(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableRangeMap.class, name)) {
			return readImmutableRangeMapInternal(p);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			RangeMap<K, V> outVal = (RangeMap<K, V>) singletonInstance(name, CREATE);
			return addToRangeMap(p, outVal, n);
		}
	}

	public static <K extends Comparable<?>, V> TreeRangeMap<K, V> readTreeRangeMap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			TreeRangeMap<K, V> outVal = TreeRangeMap.create();
			return addToRangeMap(p, outVal, n);
		}
	}

	public static <K extends Comparable<?>, V> ImmutableRangeMap<K, V> readImmutableRangeMap(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readImmutableRangeMap(p);
		}
	}

	private static <K extends Comparable<?>, V> ImmutableRangeMap<K, V> readImmutableRangeMapInternal(Parcel p) {
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

	private static <K extends Comparable<?>, V, RM extends RangeMap<K, V>> RM addToRangeMap(Parcel p, RM outVal, int size) {
		for (int i = 0; i < size; i++) {
			Range<K> range = readRange(p);
			@SuppressWarnings("unchecked")
			V value = (V) readValue(p);
			outVal.put(range, value);
		}
		return outVal;
	}

	public static <E> Set<E> readSet(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return readImmutableSortedSetInternal(p);
		} else if (isAssignableFrom(ImmutableSet.class, name)) {
			return readImmutableSetInternal(p);
		} else if (isAssignableFrom(SortedSet.class, name)) {
			return readSortedSetInternal(p, name);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			Set<E> outVal = (Set<E>) newInstance(name);
			return addToCollection(p, outVal, n);
		}
	}

	//	private static <E extends Enum<E>> EnumSet<E> readEnumSet(Parcel p) {
	//		if (p.readString().equals(NO_NAME)) {
	//			return null;
	//		} else {
	//			int n = p.readInt();
	//			EnumSet<E> outVal = new EnumSet<E>();
	//			return addToCollection(p, outVal, n);
	//		}
	//	}

	public static <E> HashSet<E> readHashSet(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			HashSet<E> outVal = new HashSet<E>();
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> ImmutableSet<E> readImmutableSet(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return readImmutableSortedSetInternal(p);
		} else {
			return readImmutableSetInternal(p);
		}
	}

	private static <E> ImmutableSet<E> readImmutableSetInternal(Parcel p) {
		int n = p.readInt();
		ImmutableSet.Builder<E> builder = ImmutableSet.builder();
		addToImmutableCollectionBuilder(p, builder, n);
		return builder.build();
	}

	public static <E> NavigableSet<E> readNavigableSet(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return readImmutableSortedSetInternal(p);
		} else {
			int n = p.readInt();
			Comparator<? super E> c = readComparator(p);
			if (c == null) {
				@SuppressWarnings("unchecked")
				NavigableSet<E> outVal = (NavigableSet<E>) newInstance(name);
				return addToCollection(p, outVal, n);
			} else {
				@SuppressWarnings("unchecked")
				NavigableSet<E> outVal = (NavigableSet<E>) newInstance(name, COMPARATOR_ARG, c);
				return addToCollection(p, outVal, n);
			}
		}
	}

	public static <E> TreeSet<E> readTreeSet(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			int n = p.readInt();
			Comparator<? super E> c = readComparator(p);
			TreeSet<E> outVal;
			if (c == null) {
				outVal = new TreeSet<E>();
			} else {
				outVal = new TreeSet<E>(c);
			}
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> SortedSet<E> readSortedSet(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return readImmutableSortedSetInternal(p);
		} else {
			return readSortedSetInternal(p, name);
		}
	}

	private static <E> SortedSet<E> readSortedSetInternal(Parcel p, String name) {
		int n = p.readInt();
		Comparator<? super E> c = readComparator(p);
		if (c == null) {
			@SuppressWarnings("unchecked")
			SortedSet<E> outVal = (SortedSet<E>) newInstance(name);
			return addToCollection(p, outVal, n);
		} else {
			@SuppressWarnings("unchecked")
			SortedSet<E> outVal = (SortedSet<E>) newInstance(name, COMPARATOR_ARG, c);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> ImmutableSortedSet<E> readImmutableSortedSet(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readImmutableSortedSetInternal(p);
		}
	}

	private static <E> ImmutableSortedSet<E> readImmutableSortedSetInternal(Parcel p) {
		int n = p.readInt();
		Comparator<E> c = readComparator(p);
		if (c == null) {
			@SuppressWarnings("unchecked")
			ImmutableSortedSet.Builder<E> builder = (ImmutableSortedSet.Builder<E>) ImmutableSortedSet.naturalOrder();
			addToImmutableCollectionBuilder(p, builder, n);
			return builder.build();
		} else {
			ImmutableSortedSet.Builder<E> builder = ImmutableSortedSet.orderedBy(c);
			addToImmutableCollectionBuilder(p, builder, n);
			return builder.build();
		}
	}

	public static <V> PatriciaTrie<V> readPatriciaTrie(Parcel p) {
		if (p.readString() == null) {
			return null;
		} else {
			return readPatriciaTrieInternal(p);
		}
	}

	private static <V> PatriciaTrie<V> readPatriciaTrieInternal(Parcel p) {
		int n = p.readInt();
		PatriciaTrie<V> outVal = new PatriciaTrie<V>();
		return addToMap(p, outVal, n);
	}

	public static <E> Collection<E> readCollection(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMultiset.class, name)) {
			return readImmutableSortedMultisetInternal(p);
		} else if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return readImmutableSortedSetInternal(p);
		} else if (isAssignableFrom(ImmutableCollection.class, name)) {
			return readImmutableCollectionInternal(p, name);
		} else if (isAssignableFrom(PriorityQueue.class, name)) {
			return readPriorityQueueInternal(p);
		} else if (isAssignableFrom(SortedMultiset.class, name)) {
			return readSortedMultisetInternal(p, name);
		} else if (isAssignableFrom(SortedSet.class, name)) {
			return readSortedSetInternal(p, name);
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			Collection<E> outVal = (Collection<E>) newInstance(name);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> ImmutableCollection<E> readImmutableCollection(Parcel p) {
		String name = p.readString();

		if (name == null) {
			return null;
		} else if (isAssignableFrom(ImmutableSortedMultiset.class, name)) {
			return readImmutableSortedMultisetInternal(p);
		} else if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return readImmutableSortedSetInternal(p);
		} else {
			return readImmutableCollectionInternal(p, name);
		}
	}

	private static <E> ImmutableCollection<E> readImmutableCollectionInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		ImmutableCollection.Builder<E> builder = (ImmutableCollection.Builder<E>) singletonInstance(name, BUILDER);
		addToImmutableCollectionBuilder(p, builder, n);
		return builder.build();
	}

	private static <E, C extends Collection<E>> C addToCollection(Parcel p, C outVal, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			outVal.add(elem);
		}
		return outVal;
	}

	private static <E> void addToImmutableCollectionBuilder(Parcel p, ImmutableCollection.Builder<E> builder, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			builder.add(elem);
		}
	}

	private static <T> Comparator<T> readComparator(Parcel p) {
		int s = p.readInt();

		if (s == -1) {
			return null;
		} else if (s == 0) {
			return p.readParcelable(ParcelUtils.class.getClassLoader());
		} else {
			@SuppressWarnings("unchecked")
			Comparator<T> c = (Comparator<T>) p.readSerializable();
			return c;
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
		case VAL_PATRICIATRIE:
			return readPatriciaTrie(p);
		case VAL_SORTEDMAP:
			return readSortedMap(p);
		case VAL_MAP2:
			return readMap(p);
		case VAL_TREEMULTIMAP:
			return readTreeMultimap(p);
		case VAL_SORTEDSETMULTIMAP:
			return readSortedSetMultimap(p);
		case VAL_MULTIMAP:
			return readMultimap(p);
		case VAL_SORTEDMULTISET:
			return readSortedMultiset(p);
		case VAL_MULTISET:
			return readMultiset(p);
		case VAL_OPTIONAL:
			return readOptional(p);
		case VAL_PRIORITYQUEUE:
			return readPriorityQueue(p);
		case VAL_QUEUE:
			return readQueue(p);
		case VAL_RANGE:
			return readRange(p);
		case VAL_RANGEMAP:
			return readRangeMap(p);
		case VAL_SORTEDSET:
			return readSortedSet(p);
		case VAL_SET:
			return readSet(p);
		case VAL_COLLECTION:
			return readCollection(p);
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
			p.writeString(null);
		} else {
			p.writeString(enumVal.getDeclaringClass().getName());
			p.writeString(enumVal.name());
		}
	}

	//	public static <K extends Enum<K>, V extends Enum<V>> void writeEnumBiMap(Parcel p, EnumBiMap<K, V> enumBiMap, Class<K> keyCls, Class<V> valueCls) {
	//
	//	}
	//	
	//	public static <K extends Enum<K>, V> void writeEnumHashBiMap(Parcel p, EnumHashBiMap<K, V> enumHashBiMap, Class<K> cls) {
	//
	//	}
	//
	//	public static <K extends Enum<K>, V> void writeEnumMap(Parcel p, EnumMap<K, V> enumMap, Class<K> cls) {
	//
	//	}
	//	
	//	public static <E extends Enum<E>> void writeEnumMultiset(Parcel p, EnumMultiset<E> enumMultiset, Class<E> cls) {
	//
	//	}
	//
	//	public static <E extends Enum<E>> void writeEnumSet(Parcel p, EnumSet<E> enumSet, Class<E> cls) {
	//
	//	}

	public static <K, V> void writeMap(Parcel p, Map<K, V> map) {
		if (map instanceof PatriciaTrie) {
			writeMap(p, map, YesNoMaybe.NO, null);
		} else {
			writeMap(p, map, YesNoMaybe.MAYBE, null);
		}
	}

	private static <K, V> void writeMap(Parcel p, Map<K, V> map, YesNoMaybe hasComparator, Comparator<? super K> comparator) {
		if (map == null) {
			p.writeString(null);
		} else {
			p.writeString(map.getClass().getName());
			p.writeInt(map.size());

			switch (hasComparator) {
			case YES:
				writeComparator(p, comparator);
				break;
			case MAYBE:
				if (map instanceof SortedMap) { // Ensure that PatriciaTries never reach this point
					SortedSet<?> sortedSet = (SortedSet<?>) map;
					Comparator<?> c = sortedSet.comparator();
					writeComparator(p, c);
				}
				break;
			case NO:
				break;
			}

			for (Map.Entry<K, V> entry : map.entrySet()) {
				writeValue(p, entry.getKey());
				writeValue(p, entry.getValue());
			}
		}
	}

	public static <K, V> void writeMultimap(Parcel p, Multimap<K, V> multimap) {
		writeMultimap(p, multimap, YesNoMaybe.MAYBE, null, YesNoMaybe.MAYBE, null);
	}

	public static <K, V> void writeMultimap(Parcel p, Multimap<K, V> multimap,
			YesNoMaybe hasKeyComparator, Comparator<? super K> keyComparator,
			YesNoMaybe hasValueComparator, Comparator<? super V> valueComparator) {
		if (multimap == null) {
			p.writeString(null);
		} else {
			p.writeString(multimap.getClass().getName());
			p.writeInt(multimap.size());

			switch (hasKeyComparator) {
			case YES:
				writeComparator(p, keyComparator);
				break;
			case MAYBE:
				if (multimap instanceof TreeMultimap) {
					TreeMultimap<?, ?> treeMultimap = (TreeMultimap<?, ?>) multimap;
					Comparator<?> keyC = treeMultimap.keyComparator();
					writeComparator(p, keyC);
				}
				break;
			case NO:
				break;
			}

			switch (hasValueComparator) {
			case YES:
				writeComparator(p, valueComparator);
				break;
			case MAYBE:	
				if (multimap instanceof SortedSetMultimap) {
					SortedSetMultimap<?, ?> sortedSetMultimap = (SortedSetMultimap<?, ?>) multimap;
					Comparator<?> valueC = sortedSetMultimap.valueComparator();
					writeComparator(p, valueC);
				}
				break;
			case NO:
				break;
			}

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

	public static <V> void writePatriciaTrie(Parcel p, PatriciaTrie<V> patriciaTrie) {
		writeMap(p, patriciaTrie, YesNoMaybe.NO, null);
	}

	public static <E> void writePriorityQueue(Parcel p, PriorityQueue<E> priorityQueue) {
		writeCollection(p, priorityQueue, YesNoMaybe.YES, priorityQueue.comparator());
	}

	public static <C extends Comparable<?>> void writeRange(Parcel p, Range<C> range) {
		if (range == null) {
			p.writeInt(-1);
		} else {
			if (!range.hasLowerBound() && !range.hasUpperBound()) {
				p.writeInt(0);
			} else if (range.hasLowerBound() && !range.hasUpperBound()) {
				p.writeInt(1);
				writeRangeEndpoint(p, range.lowerBoundType(), range.lowerEndpoint());
			} else if (!range.hasLowerBound() && range.hasUpperBound()) {
				p.writeInt(2);
				writeRangeEndpoint(p, range.upperBoundType(), range.upperEndpoint());
			} else {
				p.writeInt(3);
				writeRangeEndpoint(p, range.lowerBoundType(), range.lowerEndpoint());
				writeRangeEndpoint(p, range.upperBoundType(), range.upperEndpoint());
			}

			//			p.writeInt(1);
			//			if (range.hasLowerBound()) {
			//				writeEnum(p, range.lowerBoundType());
			//				writeValue(p, range.lowerEndpoint());
			//			} else {
			//				writeEnum(p, null);
			//				p.writeValue(null);
			//			}
			//
			//			if (range.hasUpperBound()) {
			//				writeEnum(p, range.upperBoundType());
			//				writeValue(p, range.upperEndpoint());
			//			} else {
			//				writeEnum(p, null);
			//				p.writeValue(null);
			//			}
		}
	}

	private static <C extends Comparable<?>> void writeRangeEndpoint(Parcel p, BoundType boundType, C endpoint) {
		switch (boundType) {
		case OPEN:
			writeBoolean(p, false);
			break;
		case CLOSED:
			writeBoolean(p, true);
			break;
		}
		writeValue(p, endpoint);
	}

	public static <K extends Comparable<?>, V> void writeRangeMap(Parcel p, RangeMap<K, V> rangeMap) {
		if (rangeMap == null) {
			p.writeString(null);
		} else {
			p.writeString(rangeMap.getClass().getName());
			p.writeInt(rangeMap.asMapOfRanges().size());
			for (Map.Entry<Range<K>, V> entry : rangeMap.asMapOfRanges().entrySet()) {
				writeRange(p, entry.getKey());
				writeValue(p, entry.getValue());
			}
		}
	}

	public static <K, V> void writeSortedMap(Parcel p, SortedMap<K, V> sortedMap) {
		if (sortedMap instanceof PatriciaTrie) {
			writeMap(p, sortedMap, YesNoMaybe.NO, null);
		} else {
			writeMap(p, sortedMap, YesNoMaybe.YES, sortedMap.comparator());
		}
	}

	public static <E> void writeSortedMultiset(Parcel p, SortedMultiset<E> sortedMultiset) {
		writeCollection(p, sortedMultiset, YesNoMaybe.YES, sortedMultiset.comparator());
	}

	public static <E> void writeSortedSet(Parcel p, SortedSet<E> sortedSet) {
		writeCollection(p, sortedSet, YesNoMaybe.YES, sortedSet.comparator());
	}

	public static <K, V> void writeSortedSetMultimap(Parcel p, SortedSetMultimap<K, V> sortedSetMultimap) {
		writeMultimap(p, sortedSetMultimap, YesNoMaybe.MAYBE, null, YesNoMaybe.YES, sortedSetMultimap.valueComparator());
	}

	public static <K, V> void writeTreeMultimap(Parcel p, TreeMultimap<K, V> treeMultimap) {
		writeMultimap(p, treeMultimap, YesNoMaybe.YES, treeMultimap.keyComparator(), YesNoMaybe.YES, treeMultimap.valueComparator());
	}

	public static <E> void writeCollection(Parcel p, Collection<E> collection) {
		writeCollection(p, collection, YesNoMaybe.MAYBE, null);
	}

	private static <T> void writeComparator(Parcel p, Comparator<T> comparator) {
		if (comparator == null || Ordering.natural().equals(comparator)) {
			p.writeInt(-1);
		} else if (comparator instanceof Parcelable) {
			p.writeInt(0);
			p.writeParcelable((Parcelable) comparator, 0);
		} else if (comparator instanceof Serializable) {
			p.writeInt(1);
			p.writeSerializable((Serializable) comparator);
		} else {
			p.writeInt(-1);
		}
	}

	private static <E> void writeCollection(Parcel p, Collection<E> collection, YesNoMaybe hasComparator, Comparator<? super E> comparator) {
		if (collection == null) {
			p.writeString(null);
		} else {
			p.writeString(collection.getClass().getName());
			p.writeInt(collection.size());

			switch (hasComparator) {
			case YES:
				writeComparator(p, comparator);
				break;
			case MAYBE:
				Comparator<?> c = null;
				if (collection instanceof PriorityQueue) {
					PriorityQueue<?> priorityQueue = (PriorityQueue<?>) collection;
					c = priorityQueue.comparator();
				} else if (collection instanceof SortedMultiset) {
					SortedMultiset<?> sortedMultiset = (SortedMultiset<?>) collection;
					c = sortedMultiset.comparator();
				} else if (collection instanceof SortedSet) {
					SortedSet<?> sortedSet = (SortedSet<?>) collection;
					c = sortedSet.comparator();
				}

				if (c != null) {
					writeComparator(p, c);
				}
				break;
			case NO:
				break;
			}

			for (E elem : collection) {
				writeValue(p, elem);
			}
		}
	}

	public static void writeValue(Parcel p, Object v) {
		if (v instanceof Boolean) {
			p.writeInt(VAL_BOOLEAN);
			writeBoolean(p, (Boolean) v);
		} else if (v.getClass().isEnum()) {
			p.writeInt(VAL_BOOLEAN);
			@SuppressWarnings("unchecked")
			Enum<? extends Enum<?>> e = (Enum<? extends Enum<?>>) v;
			writeEnum(p, e);
		} else if (v instanceof List) {
			p.writeInt(VAL_LIST2);
			writeCollection(p, (List<?>) v);
		} else if (v instanceof PatriciaTrie) {
			p.writeInt(VAL_PATRICIATRIE);
			writePatriciaTrie(p, (PatriciaTrie<?>) v);
		} else if (v instanceof SortedMap) {
			p.writeInt(VAL_SORTEDMAP);
			writeSortedMap(p, (SortedMap<?, ?>) v);
		} else if (v instanceof Map) {
			p.writeInt(VAL_MAP2);
			writeMap(p, (Map<?, ?>) v);
		} else if (v instanceof TreeMultimap) {
			p.writeInt(VAL_TREEMULTIMAP);
			writeTreeMultimap(p, (TreeMultimap<?, ?>) v);
		} else if (v instanceof SortedSetMultimap) {
			p.writeInt(VAL_SORTEDSETMULTIMAP);
			writeSortedSetMultimap(p, (SortedSetMultimap<?, ?>) v);
		} else if (v instanceof Multimap) {
			p.writeInt(VAL_MULTIMAP);
			writeMultimap(p, (Multimap<?, ?>) v);
		} else if (v instanceof SortedMultiset) {
			p.writeInt(VAL_SORTEDMULTISET);
			writeSortedMultiset(p, (SortedMultiset<?>) v);
		} else if (v instanceof Multiset) {
			p.writeInt(VAL_MULTISET);
			writeCollection(p, (Multiset<?>) v);
		} else if (v instanceof Optional) {
			p.writeInt(VAL_OPTIONAL);
			writeOptional(p, (Optional<?>) v);
		} else if (v instanceof PriorityQueue) {
			p.writeInt(VAL_PRIORITYQUEUE);
			writePriorityQueue(p,  (PriorityQueue<?>) v);
		} else if (v instanceof Queue) {
			p.writeInt(VAL_QUEUE);
			writeCollection(p, (Queue<?>) v);
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
		} else if (v instanceof SortedSet) {
			p.writeInt(VAL_SORTEDSET);
			writeSortedSet(p, (SortedSet<?>) v);
		} else if (v instanceof Set) {
			p.writeInt(VAL_SET);
			writeCollection(p, (Set<?>) v);
		} else if (v instanceof Collection) {
			p.writeInt(VAL_COLLECTION);
			writeCollection(p, (Collection<?>) v);
		} else { // Delegate to default writeValue()
			p.writeInt(-2);
			p.writeValue(v);
		}
	}

	private static enum YesNoMaybe { YES, MAYBE, NO };

}
