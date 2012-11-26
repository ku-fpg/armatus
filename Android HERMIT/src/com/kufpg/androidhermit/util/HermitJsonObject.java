package com.kufpg.androidhermit.util;

import java.io.InputStream;
import java.util.ArrayList;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

public class HermitJsonObject extends JSONObject {
	
	private String jsonName;
	private ArrayList<String> jsonContents = new ArrayList<String>();
	
	public HermitJsonObject(InputStream jsonStream) throws JSONException {
		super(FileIOManager.getTextFromDisk(jsonStream));
		jsonName = this.getString("fileName");
		JSONArray fileContents = (JSONArray) this.get("fileContents");
		for(int i = 0; i < fileContents.length(); i++) {
			jsonContents.add(fileContents.getString(i));
		}
	}
	
	public String getJSONName() {
		return jsonName;
	}
	
	public void setJSONName(String newName) {
		jsonName = newName;
	}
	
	public ArrayList<String> getJSONContents() {
		return jsonContents;
	}
	
	public void setJSONContents(ArrayList<String> newContents) {
		jsonContents = newContents;
	}
	
	public void writeChangesToFile(String fileLoc) {
		//TODO: Implement me
	}

}
