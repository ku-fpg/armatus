package com.kufpg.androidhermit.util;

import java.io.BufferedReader;
import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;

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

}