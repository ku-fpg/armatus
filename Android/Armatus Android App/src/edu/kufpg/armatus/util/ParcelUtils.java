package edu.kufpg.armatus.util;

import java.util.Map.Entry;
import java.util.Set;

import com.google.common.collect.ImmutableList;
import com.google.common.collect.Multimap;

import android.os.Parcel;

public class ParcelUtils {
	
	private ParcelUtils() {}
	
	public static <E> ImmutableList<E> readImmutableList(Parcel p, ClassLoader loader) {
		int n = p.readInt();
		if (n < 0) {
			return null;
		}
		
		ImmutableList.Builder<E> builder = ImmutableList.builder();
		while (n > 0) {
			@SuppressWarnings("unchecked")
			E element = (E) p.readValue(loader);
			builder.add(element);
			n--;
		}
		return builder.build();
	}
	
	public static <K, V> void readMultimap(Multimap<K, V> outVal,
			Parcel p, ClassLoader loader) {
		int n = p.readInt();
		if (n < 0) {
			return;
		}
		
		while (n > 0) {
			@SuppressWarnings("unchecked")
			K key = (K) p.readValue(loader);
			@SuppressWarnings("unchecked")
			V value = (V) p.readValue(loader);
			outVal.put(key, value);
			n--;
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
	
	public static <E> void writeSetToParcel(Set<E> set, Parcel p) {
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
