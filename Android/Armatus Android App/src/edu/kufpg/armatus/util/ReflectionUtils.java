package edu.kufpg.armatus.util;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import android.util.Log;

public final class ReflectionUtils {
	private static final String TAG = ParcelUtils.class.getSimpleName();

	private ReflectionUtils() {}

	public static Class<?> forName(String className) throws IllegalArgumentException {
		try {
			return Class.forName(className);
		} catch (ClassNotFoundException e) {
			Log.e(TAG, "Illegal access when unmarshalling: "
					+ className, e);
			throw new IllegalArgumentException("ClassNotFoundException when unmarshalling: "
					+ className);
		}
	}

	public static boolean isAssignableFrom(Class<?> cls, String asgnName) throws IllegalArgumentException {
		return cls.isAssignableFrom(forName(asgnName));
	}

	public static Object newInstance(String className) throws IllegalArgumentException {
		return newInstance(className, null, (Object[]) null);
	}

	public static Object newInstance(String className, Class<?>[] parameterTypes, Object... constructorArgs) throws IllegalArgumentException {
		try {
			Class<?> cls = forName(className);
			if (constructorArgs == null || constructorArgs.length == 0) {
				return cls.newInstance();
			} else {
				Constructor<?> constr = cls.getConstructor(parameterTypes);
				return constr.newInstance(constructorArgs);
			}
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

	public static Object singletonInstance(String className, String singletonMethodName) throws IllegalArgumentException {
		return singletonInstance(className, singletonMethodName, (Object[]) null);
	}

	public static Object singletonInstance(String className, String singletonMethodName, Object... methodArgs) throws IllegalArgumentException {
		try {
			Class<?> cls = forName(className);
			Method method = cls.getMethod(singletonMethodName);
			return method.invoke(null, methodArgs);
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
}
