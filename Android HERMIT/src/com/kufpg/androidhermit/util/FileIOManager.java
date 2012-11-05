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
import java.util.ArrayList;

import android.content.Context;
import android.util.Base64OutputStream;

public class FileIOManager {

	public static String readText(InputStream textStream) {
		InputStream inputStream = textStream;
		//InputStream inputStream = getResources().openRawResource(R.raw.internals);
		System.out.println(inputStream);
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		int i;

		try {
			i = inputStream.read();
			while (i != -1)
			{
				byteArrayOutputStream.write(i);
				i = inputStream.read();
			}
			inputStream.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return byteArrayOutputStream.toString();
	}

	public static ArrayList<String> getTextArray(InputStream textStream) {
		ArrayList<String> textArray = new ArrayList<String>();
		try {                        
			FileInputStream fileIS = (FileInputStream) textStream;          
			BufferedReader buf = new BufferedReader(new InputStreamReader(fileIS));           
			String readString;         

			while ((readString = buf.readLine()) != null) {   
				textArray.add(readString);
			}
		} catch (FileNotFoundException e) {          
			e.printStackTrace();          
		} catch (IOException e){             
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

}