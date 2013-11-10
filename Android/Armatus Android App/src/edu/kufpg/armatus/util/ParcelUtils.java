package edu.kufpg.armatus.util;

import java.util.Map.Entry;
import java.util.Set;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.common.base.Optional;
import com.google.common.collect.ImmutableList;
import com.google.common.collect.Multimap;

public class ParcelUtils {

	private ParcelUtils() {}

	public static ImmutableList<String> readImmutableStringList(Parcel p) {
		ImmutableList.Builder<String> builder = ImmutableList.builder();;
		int n = p.readInt();
		for (int i = 0; i < n; i++) {
			builder.add(p.readString());
		}
		return builder.build();
	}

	public static <E extends Parcelable> ImmutableList<E> readImmutableTypedList(Parcel p, Parcelable.Creator<E> c) {
		ImmutableList.Builder<E> builder = ImmutableList.builder();
		int n = p.readInt();
		for (int i = 0; i < n; i++) {
			if (p.readInt() != 0) {
				builder.add(c.createFromParcel(p));
			}
		}
		return builder.build();
	}

	public static <K, V> void readMultimap(Multimap<K, V> outVal,
			Parcel p, ClassLoader keyLoader, ClassLoader valueLoader) {
		int n = p.readInt();
		if (n < 0) {
			return;
		}

		while (n > 0) {
			@SuppressWarnings("unchecked")
			K key = (K) p.readValue(keyLoader);
			@SuppressWarnings("unchecked")
			V value = (V) p.readValue(valueLoader);
			outVal.put(key, value);
			n--;
		}
	}

	
	public static <T> Optional<T> readOptional(Parcel p, ClassLoader loader) {
		int s = p.readInt();
		if (s == -1) {
			return null;
		} else if (s == 0) {
			return Optional.absent();
		} else {
			@SuppressWarnings("unchecked")
			T thing = (T) p.readValue(loader);
			return Optional.of(thing);
		}
	}

	public static <E> void readSet(Set<E> outVal,
			Parcel p, ClassLoader loader) {
		int n = p.readInt();
		if (n < 0) {
			return;
		}

		while (n > 0) {
			@SuppressWarnings("unchecked")
			E element = (E) p.readValue(loader);
			outVal.add(element);
			n--;
		}
	}

	public static <K, V> void writeMultimap(Multimap<K, V> multimap, Parcel p) {
		if (multimap == null) {
			p.writeInt(-1);
			return;
		}

		p.writeInt(multimap.size());
		for (Entry<K, V> e : multimap.entries()) {
			p.writeValue(e.getKey());
			p.writeValue(e.getValue());
		}
	}

	public static <T> void writeOptional(Optional<T> opt, Parcel p) {
		if (opt == null) {
			p.writeInt(-1);
			return;
		} else if (opt.isPresent()) {
			p.writeInt(1);
			p.writeValue(opt.get());
		} else {
			p.writeInt(0);
			return;
		}
	}

	public static <E> void writeSet(Set<E> set, Parcel p) {
		if (set == null) {
			p.writeInt(-1);
			return;
		}

		p.writeInt(set.size());
		for (E obj : set) {
			p.writeValue(obj);
		}
	}

}
