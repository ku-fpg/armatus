package com.kufpg.armatus.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import org.json.JSONException;
import org.json.JSONObject;

public class JsonUtils {

	public static void saveJsonFile(JSONObject obj, String path) {
		try {
			File file = new File(path);
			file.getParentFile().mkdirs();
			FileWriter writer = new FileWriter(path);
			writer.write(obj.toString());
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public static JSONObject openJsonFile(String path) throws FileNotFoundException, JSONException {
		return new JSONObject(openTextFile(path)); 
	}

	public static String openTextFile(String path) throws FileNotFoundException {
		BufferedReader br = null;
		try {
			br = new BufferedReader(new FileReader(path));
			StringBuilder sb = new StringBuilder();
			String line = br.readLine();

			while (line != null) {
				sb.append(line + "\n");
				line = br.readLine();
			}

			br.close();
			return sb.toString();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
	}

}