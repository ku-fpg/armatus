package com.kufpg.androidhermit.util;

import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.util.ArrayList;

import android.content.Context;
import android.util.Base64OutputStream;
import android.widget.Toast;

public class FileIOManager {

	public static ArrayList<String> getTextArray(InputStream textStream) {
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

	public static boolean saveTextArray(ArrayList<String> textArray, String path, String fileName, Context context) {
		OutputStream os = null;

		String str = arrayListToString(textArray);
		File file = new File(path);
		file.mkdirs();
		String fileLoc = path + "/" + fileName;
		try {
			os = new BufferedOutputStream(new FileOutputStream(fileLoc, true));
			os.write(str.getBytes());
			os.flush();
			os.close();
			return true;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

	public static String arrayListToString(ArrayList<String> al) {
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		try {
			new ObjectOutputStream(out).writeObject(al);
			byte[] data = out.toByteArray();
			out.close();

			out = new ByteArrayOutputStream();
			Base64OutputStream b64 = new Base64OutputStream(out,0);
			b64.write(data);
			b64.close();
			out.close();

			return new String(out.toByteArray());
		} catch (IOException e) {
			e.printStackTrace();
		}
		return null;
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