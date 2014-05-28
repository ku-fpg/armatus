package edu.kufpg.armatus.util;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;

/**
 * Utility class containing methods useful for JSON I/O.
 */
public class JsonUtils {

	private JsonUtils() {}

	/**
	 * Saves a {@link JSONObject} to disk.
	 * @param obj The {@code JSONObject} to save.
	 * @param path The string representation of the file path to save to.
	 */
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

	/**
	 * Returns a {@link JSONObject} read from a saved file.
	 * @param path The string representation of the file path from which to open the
	 * {@code JSONObject} file.
	 * @return The {@code JSONObject} from the saved file.
	 * @throws FileNotFoundException If {@code path} does not exist.
	 * @throws JSONException if the {@code JSONObject} in the saved file is invalid.
	 */
	public static JSONObject openJsonFile(String path) throws FileNotFoundException, JSONException {
		return new JSONObject(openTextFile(path)); 
	}

	/**
	 * Returns a string representation of the contents of a saved file.
	 * @param path The string representation of the file path to open.
	 * @return The contents of the saved file.
	 * @throws FileNotFoundException if {@code path} does not exist.
	 */
	public static String openTextFile(String path) throws FileNotFoundException {
        BufferedReader br = new BufferedReader(new FileReader(path));
		try {

            StringBuilder sb = new StringBuilder();
			String line = br.readLine();

			while (line != null) {
				sb.append(line).append('\n');
				line = br.readLine();
			}

			return sb.toString();
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
            try {
                br.close();
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
		return null;
	}

}