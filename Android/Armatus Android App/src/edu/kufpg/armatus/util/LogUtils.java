package edu.kufpg.armatus.util;

import android.util.Log;

public final class LogUtils {

	private LogUtils() {}
	
	public static <T> T d(String tag, T obj) {
		Log.d(tag, obj.toString());
		return obj;
	}
	
	public static <T> T d(String tag, String msg, T obj) {
		Log.d(tag, msg);
		return obj;
	}
	
	public static <T> T e(String tag, T obj) {
		Log.e(tag, obj.toString());
		return obj;
	}
	
	public static <T> T e(String tag, String msg, T obj) {
		Log.e(tag, msg);
		return obj;
	}
	
	public static <T> T i(String tag, T obj) {
		Log.i(tag, obj.toString());
		return obj;
	}
	
	public static <T> T i(String tag, String msg, T obj) {
		Log.i(tag, msg);
		return obj;
	}
	
	public static <T> T v(String tag, T obj) {
		Log.v(tag, obj.toString());
		return obj;
	}
	
	public static <T> T v(String tag, String msg, T obj) {
		Log.v(tag, msg);
		return obj;
	}
	
	public static <T> T w(String tag, T obj) {
		Log.w(tag, obj.toString());
		return obj;
	}
	
	public static <T> T w(String tag, String msg, T obj) {
		Log.w(tag, msg);
		return obj;
	}
	
	public static <T> T wtf(String tag, T obj) {
		Log.wtf(tag, obj.toString());
		return obj;
	}
	
	public static <T> T wtf(String tag, String msg, T obj) {
		Log.wtf(tag, msg);
		return obj;
	}
	
}
