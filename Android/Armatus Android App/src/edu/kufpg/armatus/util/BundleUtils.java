package edu.kufpg.armatus.util;

import java.util.Collection;
import java.util.List;
import java.util.Map;

import android.os.Bundle;
import android.os.Parcelable;

import com.google.common.collect.Multimap;

public class BundleUtils {
	private static final String SIZE = "Size", KEY = "Key", VALUE = "Value";

	private BundleUtils() {}

	public static <E extends Parcelable> void putParcelableList(Bundle b, String key, List<E> value) {
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
	
	public static <K extends Parcelable, V extends Parcelable> void putParcelableMap(Bundle b, String key, Map<K, V> value) {
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
	
	public static <K extends Parcelable, V extends Parcelable> void putParcelableMultimap(Bundle b, String key, Multimap<K, V> value) {
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
	
	public static <E extends Parcelable> void getParcelableList(Bundle b, String key, List<E> outVal) {
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

	public static <K extends Parcelable, V extends Parcelable> void getParcelableMap(Bundle b, String key, Map<K, V> outVal) {
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
	
	public static <K extends Parcelable, V extends Parcelable> void getParcelableMultimap(Bundle b, String key, Multimap<K, V> outVal) {
		int n = b.getInt(key + SIZE);
		int i = 0;
		
		outVal.clear();
		for (; i < n; i++) {
			K k = b.getParcelable(key + KEY + i);
			int q = b.getInt(key + KEY + SIZE + i);
			for (int j = 0; j < q; j++) {
				V v = b.getParcelable(key + VALUE + i + j);
				outVal.put(k, v);
			}
		}
		
	}
	
}
