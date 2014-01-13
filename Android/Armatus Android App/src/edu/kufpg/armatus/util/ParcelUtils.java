package edu.kufpg.armatus.util;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.NavigableSet;
import java.util.Set;
import java.util.SortedSet;
import java.util.Stack;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.Vector;

import org.apache.commons.collections4.trie.PatriciaTrie;

import android.os.Parcel;
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
import com.google.common.collect.ImmutableSortedMultiset;
import com.google.common.collect.ImmutableSortedSet;
import com.google.common.collect.LinkedHashMultimap;
import com.google.common.collect.LinkedHashMultiset;
import com.google.common.collect.LinkedListMultimap;
import com.google.common.collect.ListMultimap;
import com.google.common.collect.Multimap;
import com.google.common.collect.Multiset;
import com.google.common.collect.Range;
import com.google.common.collect.RangeMap;
import com.google.common.collect.SetMultimap;
import com.google.common.collect.SortedMultiset;
import com.google.common.collect.SortedSetMultimap;
import com.google.common.collect.TreeMultimap;
import com.google.common.collect.TreeMultiset;
import com.google.common.collect.TreeRangeMap;

public class ParcelUtils {
	private static final String TAG = ParcelUtils.class.getSimpleName();
	private static final String NO_NAME = "!@#$%^&*()";
	private static final String CREATE = "create";
	private static final String BUILDER = "builder";

	private static final int VAL_BOOLEAN = 25;
	private static final int VAL_ENUM = 26;
	private static final int VAL_LIST2 = 27;
	private static final int VAL_MAP2 = 28;
	private static final int VAL_MULTIMAP = 29;
	private static final int VAL_MULTISET = 30;
	private static final int VAL_OPTIONAL = 31;
	private static final int VAL_RANGE = 32;
	private static final int VAL_RANGEMAP = 33;
	private static final int VAL_SET = 34;
	private static final int VAL_COLLECTION = 35;

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

	private static <E, C extends Collection<E>> C addToCollection(Parcel p, C outVal, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			outVal.add(elem);
		}
		return outVal;
	}

	private static <E, ICB extends ImmutableCollection.Builder<E>> void addToImmutableCollectionBuilder(Parcel p, ICB builder, int size) {
		for (int i = 0; i < size; i++) {
			@SuppressWarnings("unchecked")
			E elem = (E) readValue(p);
			builder.add(elem);
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

	public static <E> ArrayList<E> readArrayList(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			ArrayList<E> outVal = new ArrayList<E>(n);
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> LinkedList<E> readLinkedList(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			LinkedList<E> outVal = new LinkedList<E>();
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> Stack<E> readStack(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			Stack<E> outVal = new Stack<E>();
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> Vector<E> readVector(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			Vector<E> outVal = new Vector<E>(n);
			return addToCollection(p, outVal, n);
		}
	}

	private static <E> List<E> readListInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		List<E> outVal = (List<E>) newInstance(name);
		return addToCollection(p, outVal, n);
	}

	public static <E> ImmutableList<E> readImmutableList(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
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

		if (isAssignableFrom(ImmutableMap.class, name)) {
			return readImmutableMapInternal(p);
		} else if (isAssignableFrom(ImmutableBiMap.class, name)) {
			return readImmutableBiMapInternal(p);
		} else if (isAssignableFrom(BiMap.class, name)) {
			return readBiMapInternal(p, name);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readMapInternal(p, name);
		}
	}

	public static <K, V> BiMap<K, V> readBiMap(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableBiMap.class, name)) {
			return readImmutableBiMapInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readBiMapInternal(p, name);
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
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			HashBiMap<K, V> outVal = HashBiMap.create(n);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> HashMap<K, V> readHashMap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			HashMap<K, V> outVal = new HashMap<K, V>(n);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> Hashtable<K, V> readHashtable(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			Hashtable<K, V> outVal = new Hashtable<K, V>(n);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> LinkedHashMap<K, V> readLinkedHashMap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			LinkedHashMap<K, V> outVal = new LinkedHashMap<K, V>(n);
			return addToMap(p, outVal, n);
		}
	}

	public static <K, V> TreeMap<K, V> readTreeMap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			TreeMap<K, V> outVal = new TreeMap<K, V>();
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
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			return readImmutableMap(p);
		}
	}

	private static <K, V> ImmutableMap<K, V> readImmutableMapInternal(Parcel p) {
		int n = p.readInt();
		ImmutableMap.Builder<K, V> builder = ImmutableMap.builder();
		addToImmutableMapBuilder(p, builder, n);
		return builder.build();
	}

	public static <K, V> ImmutableBiMap<K, V> readImmutableBiMap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
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

	private static <K, V, IMB extends ImmutableMap.Builder<K, V>> void addToImmutableMapBuilder(Parcel p, IMB builder, int size) {
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

	public static <K, V> ArrayListMultimap<K, V> readArrayListMultimap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			ArrayListMultimap<K, V> outVal = ArrayListMultimap.create(n, 3);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> LinkedListMultimap<K, V> readLinkedListMultimap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			LinkedListMultimap<K, V> outVal = LinkedListMultimap.create(n);
			return addToMultimap(p, outVal, n);
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

	public static <K, V> HashMultimap<K, V> readHashMultimap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			HashMultimap<K, V> outVal = HashMultimap.create(n, 2);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K, V> LinkedHashMultimap<K, V> readLinkedHashMultimap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			LinkedHashMultimap<K, V> outVal = LinkedHashMultimap.create(n, 2);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K extends Comparable<? super K>, V extends Comparable<? super V>> SortedSetMultimap<K, V> readSortedSetMultimap(Parcel p) {
		String name = p.readString();
		if (name.equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			@SuppressWarnings("unchecked")
			SortedSetMultimap<K, V> outVal = (SortedSetMultimap<K, V>) singletonInstance(name, CREATE);
			return addToMultimap(p, outVal, n);
		}
	}

	public static <K extends Comparable<? super K>, V extends Comparable<? super V>> TreeMultimap<K, V> readTreeMultimap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			TreeMultimap<K, V> outVal = TreeMultimap.create();
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
		if (p.readString().equals(NO_NAME)) {
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
		if (p.readString().equals(NO_NAME)) {
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

	private static <K, V, IMB extends ImmutableMultimap.Builder<K, V>> void addToImmutableMultimapBuilder(Parcel p, IMB builder, int size) {
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

	private static <K, V> ListMultimap<K, V> readListMultimapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		ListMultimap<K, V> outVal = (ListMultimap<K, V>) singletonInstance(name, CREATE);
		return addToMultimap(p, outVal, n);
	}

	private static <K, V> SetMultimap<K, V> readSetMultimapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		SetMultimap<K, V> outVal = (SetMultimap<K, V>) singletonInstance(name, CREATE);
		return addToMultimap(p, outVal, n);
	}

	private static <K, V> Multimap<K, V> readMultimapInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		Multimap<K, V> outVal = (Multimap<K, V>) singletonInstance(name, CREATE);
		return addToMultimap(p, outVal, n);
	}

	public static <E> Multiset<E> readMultiset(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableSortedMultiset.class, name)) {
			@SuppressWarnings("unchecked")
			Multiset<E> multiset = (Multiset<E>) readImmutableSortedMultisetInternal(p);
			return multiset;
		} else if (isAssignableFrom(ImmutableMultiset.class, name)) {
			return readImmutableMultisetInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readMultisetInternal(p, name);
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
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			HashMultiset<E> outVal = HashMultiset.create(n);
			return addToCollection(p, outVal, n);
		}
	}
	
	public static <E> LinkedHashMultiset<E> readLinkedHashMultiset(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			LinkedHashMultiset<E> outVal = LinkedHashMultiset.create(n);
			return addToCollection(p, outVal, n);
		}
	}

	private static <E> Multiset<E> readMultisetInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		Multiset<E> outVal = (Multiset<E>) singletonInstance(name, CREATE);
		return addToCollection(p, outVal, n);
	}
	
	public static <E extends Comparable<E>> SortedMultiset<E> readSortedMultiset(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableSortedMultiset.class, name)) {
			return readImmutableSortedMultisetInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readSortedMultisetInternal(p, name);
		}
	}
	
	public static <E extends Comparable<? super E>> TreeMultiset<E> readTreeMultiset(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			TreeMultiset<E> outVal = TreeMultiset.create();
			return addToCollection(p, outVal, n);
		}
	}
	
	private static <E extends Comparable<? super E>> SortedMultiset<E> readSortedMultisetInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		SortedMultiset<E> outVal = (SortedMultiset<E>) singletonInstance(name, CREATE);
		return addToCollection(p, outVal, n);
	}

	public static <E> ImmutableMultiset<E> readImmutableMultiset(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
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

	public static <E extends Comparable<E>> ImmutableSortedMultiset<E> readImmutableSortedMultiset(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			return readImmutableSortedMultisetInternal(p);
		}
	}

	private static <E extends Comparable<E>> ImmutableSortedMultiset<E> readImmutableSortedMultisetInternal(Parcel p) {
		int n = p.readInt();
		ImmutableSortedMultiset.Builder<E> builder = ImmutableSortedMultiset.naturalOrder();
		addToImmutableCollectionBuilder(p, builder, n);
		return builder.build();
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

	public static <K extends Comparable<? super K>, V> TreeRangeMap<K, V> readTreeRangeMap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			TreeRangeMap<K, V> outVal = TreeRangeMap.create();
			return addToRangeMap(p, outVal, n);
		}
	}

	public static <K extends Comparable<? super K>, V> ImmutableRangeMap<K, V> readImmutableRangeMap(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			return readImmutableRangeMap(p);
		}
	}

	private static <K extends Comparable<? super K>, V> ImmutableRangeMap<K, V> readImmutableRangeMapInternal(Parcel p) {
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
		return addToRangeMap(p, outVal, n);
	}

	private static <K extends Comparable<? super K>, V, RM extends RangeMap<K, V>> RM addToRangeMap(Parcel p, RM outVal, int size) {
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

		if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			@SuppressWarnings("unchecked")
			Set<E> set = (Set<E>) readImmutableSortedSetInternal(p);
			return set;
		} else if (isAssignableFrom(ImmutableSet.class, name)) {
			return readImmutableSetInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readSetInternal(p, name);
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
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			HashSet<E> outVal = new HashSet<E>();
			return addToCollection(p, outVal, n);
		}
	}

	public static <E> ImmutableSet<E> readImmutableSet(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
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

	private static <E> Set<E> readSetInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		Set<E> outVal = (Set<E>) newInstance(name);
		return addToCollection(p, outVal, n);
	}

	public static <E extends Comparable<? super E>> NavigableSet<E> readNavigableSet(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return readImmutableSortedSetInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readNavigableSetInternal(p, name);
		}
	}

	public static <E extends Comparable<? super E>> TreeSet<E> readTreeSet(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			TreeSet<E> outVal = new TreeSet<E>();
			return addToCollection(p, outVal, n);
		}
	}

	private static <E extends Comparable<? super E>> NavigableSet<E> readNavigableSetInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		NavigableSet<E> outVal = (NavigableSet<E>) newInstance(name);
		return addToCollection(p, outVal, n);
	}

	public static <E extends Comparable<? super E>> SortedSet<E> readSortedSet(Parcel p) {
		String name = p.readString();

		if (isAssignableFrom(ImmutableSortedSet.class, name)) {
			return readImmutableSortedSetInternal(p);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readSortedSetInternal(p, name);
		}
	}

	public static <E extends Comparable<? super E>> ImmutableSortedSet<E> readImmutableSortedSet(Parcel p) {
		if (p.readString().equals(NO_NAME)) {
			return null;
		} else {
			return readImmutableSortedSetInternal(p);
		}
	}

	private static <E extends Comparable<? super E>> ImmutableSortedSet<E> readImmutableSortedSetInternal(Parcel p) {
		int n = p.readInt();
		ImmutableSortedSet.Builder<E> builder = ImmutableSortedSet.naturalOrder();
		addToImmutableCollectionBuilder(p, builder, n);
		return builder.build();
	}

	private static <E> SortedSet<E> readSortedSetInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		SortedSet<E> outVal = (SortedSet<E>) newInstance(name);
		return addToCollection(p, outVal, n);
	}

	public static <V> PatriciaTrie<V> readPatriciaTrie(Parcel p) {
		String name = p.readString();
		
		if (name.equals(NO_NAME)) {
			return null;
		} else {
			int n = p.readInt();
			PatriciaTrie<V> outVal = new PatriciaTrie<V>();
			return addToMap(p, outVal, n);
		}
	}
	
	public static <E> Collection<E> readCollection(Parcel p) {
		String name = p.readString();
		
		if (isAssignableFrom(ImmutableCollection.class, name)) {
			return readImmutableCollectionInternal(p, name);
		} else if (name.equals(NO_NAME)) {
			return null;
		} else {
			return readCollectionInternal(p, name);
		}
	}
	
	private static <E> Collection<E> readCollectionInternal(Parcel p, String name) {
		int n = p.readInt();
		@SuppressWarnings("unchecked")
		Collection<E> outVal = (Collection<E>) newInstance(name);
		return addToCollection(p, outVal, n);
	}
	
	public static <E> ImmutableCollection<E> readImmutableCollection(Parcel p) {
		String name = p.readString();
		
		if (p.readString().equals(NO_NAME)) {
			return null;
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
		case VAL_MULTISET:
			return readMultiset(p);
		case VAL_OPTIONAL:
			return readOptional(p);
		case VAL_RANGE:
			return readRange(p);
		case VAL_RANGEMAP:
			return readRangeMap(p);
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
			p.writeString(NO_NAME);
		} else {
			p.writeString(enumVal.getDeclaringClass().getName());
			p.writeString(enumVal.name());
		}
	}

	public static <E> void writeList(Parcel p, List<E> list) {
		writeCollection(p, list);
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

	public static <E> void writeMultiset(Parcel p, Multiset<E> multiset) {
		writeCollection(p, multiset);
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
		writeCollection(p, set);
	}

	public static <E> void writeCollection(Parcel p, Collection<E> collection) {
		if (collection == null) {
			p.writeString(NO_NAME);
		} else {
			p.writeString(collection.getClass().getName());
			p.writeInt(collection.size());
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
		} else if (v instanceof Multiset) {
			p.writeInt(VAL_MULTISET);
			writeMultiset(p, (Multiset<?>) v);
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
		} else if (v instanceof Collection) {
			p.writeInt(VAL_COLLECTION);
			writeCollection(p, (Collection<?>) v);
		} else { // Delegate to default writeValue()
			p.writeInt(-2);
			p.writeValue(v);
		}
	}

}
