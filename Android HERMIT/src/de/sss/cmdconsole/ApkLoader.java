/*
 *  Copyright 2011 Seto Chi Lap (setosoft@gmail.com)
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package de.sss.cmdconsole;

import java.lang.reflect.Method;
import java.io.File;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.util.zip.*;
import java.util.HashMap;


import dalvik.system.DexClassLoader;
import de.sss.cmdconsole.builtincmd.FileSys;
import de.sss.cmdconsole.common.*;

import android.util.Log;

public class ApkLoader {
	public static final String MAINMETHODNAME = "main";
	private static final String ENTRYPOINT_DECLARATION = "res/raw/entrypoint.txt";
	private static final String APKCACHEDIR = "apkCache";
	private static final String DOTDEX = ".dex";
	private static final String DEXEXT = "dex";
	private static final String APKEXT = "apk";

	private ApkLoader() {
	}

	public static String getDexCacheDir() {
		String appDataRoot = FileSys.getPrivateDataRoot();
		if (!appDataRoot.endsWith(ConstantData.DIRSEPARATOR))
			appDataRoot += ConstantData.DIRSEPARATOR;

		return appDataRoot + APKCACHEDIR;
	}

	/**
	 * Load the entry point method
	 * 
	 * @param apkPath
	 * @return
	 */
	public static Method loadEntryPoint(String apkPath) throws IOException,
			ClassNotFoundException, NoSuchMethodException {
		if (!FileSys.fileExists(apkPath, true))
			throw new IOException(apkPath + " is not a file or does not exist");

		deleteCachedDex(apkPath);

		String entryPointClassName = readEntryPointClass(apkPath);
		if (entryPointClassName == null)
			throw new IOException("Read entry point class error.");

		DexClassLoader classLoader = new DexClassLoader(apkPath,
				getDexCacheDir(), null, ApkLoader.class.getClassLoader());

		Class<?> myClass = classLoader.loadClass(entryPointClassName);
		HashMap<Integer, Object> dummy = new HashMap<Integer, Object>();
		Method m = myClass.getDeclaredMethod(MAINMETHODNAME, dummy.getClass());

		return m;
	}

	/**
	 * Delete the cached dex file. Since if a new apk is to be run but a stale
	 * cached dex already exists, this console app will be signaled to die
	 * unconditionally.
	 * 
	 * @param apkPath
	 *            -- the apk file path ready to run
	 */
	private static void deleteCachedDex(String apkPath) {
		int slashPos = apkPath.lastIndexOf(ConstantData.DIRSEPARATOR);
		String fileName = apkPath.substring(slashPos + 1);
		int dotPos = fileName.lastIndexOf(".");
		String extension = (dotPos >= 0 && dotPos != fileName.length() - 1) ? fileName
				.substring(dotPos + 1) : null;

		do {
			if (extension != null) {
				if (extension.equalsIgnoreCase(APKEXT)) {
					fileName = fileName.substring(0, dotPos) + "." + DEXEXT;
					break;
				}
			}

			// all other cases
			fileName += ".";
			fileName += DEXEXT;
		} while (false);

		String dexFile = getDexCacheDir() + "/" + fileName;
		if (FileSys.fileExists(dexFile, true)) {
			Log.d("ApkLoader", "deleting " + dexFile);
			FileSys.deleteFile(dexFile);
		}
	}

	/**
	 * Read the class name with 'main' method declared, which should be located
	 * at ENTRYPOINT_DECLARATION inside the archive.
	 * 
	 * @param apkPath
	 * @return
	 * @throws IOException
	 */
	private static String readEntryPointClass(String apkPath)
			throws IOException {
		ZipFile apkFile = new ZipFile(apkPath); // apk is itself a zip archive

		ZipEntry zipEntry = apkFile.getEntry(ENTRYPOINT_DECLARATION);
		if (zipEntry == null) {
			apkFile.close();
			throw new IOException(
					"Entry point declaration file does not exist -- "
							+ ENTRYPOINT_DECLARATION);
		}

		InputStream inEntry = apkFile.getInputStream(zipEntry);
		BufferedReader reader = new BufferedReader(new InputStreamReader(
				inEntry));
		String entryPointClass = reader.readLine();

		reader.close();
		inEntry.close();
		apkFile.close();

		return CFunc.trimString(entryPointClass);
	}

	/**
	 * Delete all cached dex files
	 */
	public static void deleteAllCachedDex() {
		File cachedDir = new File(getDexCacheDir());
		File[] list = cachedDir.listFiles();

		if (list != null && list.length > 0) {
			for (File f : list) {
				if (f.getName().endsWith(DOTDEX))
					f.delete();
			}
		}
	}
}
