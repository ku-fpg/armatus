package com.kufpg.androidhermit.util;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.util.ArrayList;

public class FileIOManager {

	public static ArrayList<String> getTextArrayFromDisk(InputStream textStream) {
		ArrayList<String> textArray = new ArrayList<String>();
		try {                        
			FileInputStream fileIS = (FileInputStream) textStream;          
			BufferedReader buf = new BufferedReader(new InputStreamReader(fileIS));           
			String readString;
			while ((readString = buf.readLine()) != null) {   
				textArray.add(readString);
			}
			buf.close();
		} catch (FileNotFoundException e) {          
			e.printStackTrace();          
		} catch (IOException e){             
			e.printStackTrace();          
		}    
		return textArray;
	}
	
	public static String getTextFromDisk(InputStream textStream) {
		String text = "";
		try {                        
			FileInputStream fileIS = (FileInputStream) textStream;          
			BufferedReader buf = new BufferedReader(new InputStreamReader(fileIS));           
			String readString;
			while ((readString = buf.readLine()) != null) {   
				text += readString + "\n";
			}
			buf.close();
		} catch (FileNotFoundException e) {          
			e.printStackTrace();          
		} catch (IOException e){             
			e.printStackTrace();          
		}    
		return text;
	}

	public static ArrayList<String> getTextArrayFromUrl(String textUrlLoc) {
		ArrayList<String> textArray = new ArrayList<String>();
		try {
			URL textUrl = new URL(textUrlLoc);
			BufferedReader buf = new BufferedReader(new InputStreamReader(textUrl.openStream()));
			String readString;
			while ((readString = buf.readLine()) != null) {
				textArray.add(readString);
			}
			buf.close();
		} catch (MalformedURLException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return textArray;
	}

	public static boolean saveTextArray(ArrayList<String> textArray, String path, String fileName) {
		FileWriter writer = null;
		try {
			writer = new FileWriter(path + "/" + fileName);
			for(String str: textArray) {
				writer.write(str + System.getProperty("line.separator"));
			}
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		} finally {
			try {
				writer.close();
				return true;
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		return false;
	}

	public static boolean isTextFile(String urlLoc) {
		URL url = null;
		try {
			url = new URL(urlLoc);
			HttpURLConnection urlc = (HttpURLConnection)url.openConnection();
			urlc.setAllowUserInteraction(false);
			urlc.setDoInput(true);
			urlc.setDoOutput(false);
			urlc.setUseCaches(true);
			urlc.setRequestMethod("HEAD");
			urlc.connect();
			String mime = urlc.getContentType();
			if(mime != null) {
				mime = mime.replaceAll(";[^;]*$", ""); //Get rid of some extra stuff after the MIME type
				if(mime.equals("text/plain")) {
					return true;
				}
			}
		} catch(MalformedURLException e) {
			e.printStackTrace();
		} catch (ProtocolException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

}