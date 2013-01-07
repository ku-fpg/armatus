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
package de.sss.cmdconsole.builtincmd;

import java.util.LinkedHashMap;

import android.content.ContentValues;
import android.content.Context;
import android.database.Cursor;
import android.database.sqlite.SQLiteDatabase;
import android.database.sqlite.SQLiteOpenHelper;

import de.sss.cmdconsole.common.CFunc;

public class CmdSettings {
	private static final String TRUEVALUE = "T";
	private static final String FALSEVALUE = "F";

	private static final int DEFAULT_HISTORY_NUM = 200;

	private static final String DATABASE_NAME = "consoleSetting.db";
	private static final int DATABASE_VERSION = 1;

	/* table name */
	private static final String TBL_CMD_HISTORY = "cmdhistory";
	private static final String TBL_SETTING = "setting";

	private enum DBName {
		history_id, cmd_string, // cmd history columns
		s_name, s_value // setting columns
	};

	public enum Setting {
		ls_full_mode, console_font_size,
	};

	/* command history table columns */
	private static final String[][] col_cmdHistory = new String[][] {
			new String[] { DBName.history_id.name(), "INTEGER PRIMARY KEY" },
			new String[] { DBName.cmd_string.name(), "TEXT" } };

	private static final String[][] col_setting = new String[][] {
			new String[] { DBName.s_name.name(), "TEXT PRIMARY KEY" },
			new String[] { DBName.s_value.name(), "TEXT" } };

	/**
	 * This class helps open, create, and upgrade the database file.
	 */
	private static class DatabaseHelper extends SQLiteOpenHelper {

		DatabaseHelper(Context context) {
			super(context, DATABASE_NAME, null, DATABASE_VERSION);
		}

		private static String getCreateTblSql(String tblName, String[][] columns) {
			StringBuilder sb = new StringBuilder(128);
			sb.append("CREATE TABLE ").append(tblName).append(" (");
			for (int i = 0; i < columns.length; ++i) {
				sb.append(columns[i][0]).append(" ").append(columns[i][1]);
				if (i < columns.length - 1)
					sb.append(",");
			}
			sb.append(");");
			return sb.toString();
		}

		/**
		 * Called when database is first created
		 * 
		 * @param db
		 */
		@Override
		public void onCreate(SQLiteDatabase db) {
			// create name-value setting table
			db.execSQL(getCreateTblSql(TBL_SETTING, col_setting));
			// create command history table
			db.execSQL(getCreateTblSql(TBL_CMD_HISTORY, col_cmdHistory));
		}

		@Override
		public void onUpgrade(SQLiteDatabase db, int oldVersion, int newVersion) {
			/* not implemented */
		}

	}

	private DatabaseHelper dbOpenHelper; // /< help to open/create database
	private SQLiteDatabase dbInst; // /< opened database connection

	public CmdSettings() {
		Context context = CFunc.getAppInst();
		dbOpenHelper = new DatabaseHelper(context);
		dbInst = dbOpenHelper.getWritableDatabase(); // always writable

		// clear those oldest command upon start up
		clearHistory(getMaxCmdID() - DEFAULT_HISTORY_NUM);
	}

	/**
	 * Close the underlying database connection
	 */
	public void close() {
		if (dbInst != null) {
			dbInst.close();
			dbInst = null;
		}
		if (dbOpenHelper != null) {
			dbOpenHelper.close();
			dbOpenHelper = null;
		}
	}

	// /////////////// set functions for general setting /////////////////////

	public void setValue(Setting setting, Object val) {
		String saveVal = null;
		if (val instanceof Boolean)
			saveVal = ((Boolean) val) ? TRUEVALUE : FALSEVALUE;
		else if (val instanceof Integer)
			saveVal = String.valueOf(val);
		else if (val instanceof Long)
			saveVal = String.valueOf(val);
		else if (val instanceof String)
			saveVal = (String) val;

		if (saveVal != null)
			setValue(setting.name(), saveVal);
	}

	public void setValue(String name, String value) {
		ContentValues values = new ContentValues();
		values.put(DBName.s_value.name(), value);

		if (dbInst.update(TBL_SETTING, values, DBName.s_name.name() + "='"
				+ name + "'", null) != 1) {
			values.put(DBName.s_name.name(), name);
			dbInst.insert(TBL_SETTING, null, values);
		}
	}

	// /////////////// get functions for general setting /////////////////////

	public boolean getBooleanVal(Setting setting) {
		String val = getValue(setting.name());
		if (val == null)
			return false;

		return val.equalsIgnoreCase(TRUEVALUE);
	}

	public int getIntVal(Setting setting) {
		String val = getValue(setting.name());
		if (val == null)
			return 0;

		return CFunc.parseInt(val);
	}

	public long getLongVal(Setting setting) {
		String val = getValue(setting.name());
		if (val == null)
			return 0;

		return CFunc.parseLong(val);
	}

	public String getStringVal(Setting setting) {
		return getValue(setting.name());
	}

	public String getValue(String name) {
		String val = null;
		Cursor c = dbInst.query(TBL_SETTING,
				new String[] { DBName.s_value.name() }, DBName.s_name.name()
						+ "='" + name + "'", null, null, null, null);
		if (c != null && c.moveToFirst()) {
			val = c.getString(0);
		}
		c.close();

		return val;
	}

	// ////////////////// command history ////////////////////////////////

	public void saveCommand(String cmdString) {
		int newID = getMaxCmdID() + 1;
		ContentValues values = new ContentValues();
		values.put(DBName.history_id.name(), newID);
		values.put(DBName.cmd_string.name(), cmdString);

		dbInst.insert(TBL_CMD_HISTORY, null, values);
	}

	public String getCommandById(int id) {
		String val = null;
		Cursor c = dbInst.query(TBL_CMD_HISTORY,
				new String[] { DBName.cmd_string.name() },
				DBName.history_id.name() + "=" + id, null, null, null, null);
		if (c != null && c.moveToFirst()) {
			val = c.getString(0);
		}
		c.close();

		return val;
	}

	public LinkedHashMap<Integer, String> getHistoryList(int nrItems) {
		int maxID = getMaxCmdID();
		int startID = (nrItems > 0) ? (maxID - nrItems)
				: (maxID - DEFAULT_HISTORY_NUM);

		Cursor c = dbInst.query(TBL_CMD_HISTORY, null, DBName.history_id.name()
				+ ">" + startID, null, null, null, DBName.history_id.name()
				+ " ASC");

		LinkedHashMap<Integer, String> resultList = null;
		int nrRow = c.getCount();
		if (nrRow > 0 && c.moveToFirst()) {
			resultList = new LinkedHashMap<Integer, String>();
			do {
				resultList.put(c.getInt(0), c.getString(1));
			} while (c.moveToNext());
		}
		c.close();

		return resultList;
	}

	private int getMaxCmdID() {
		Cursor c = dbInst.rawQuery("select max(" + DBName.history_id.name()
				+ ") from " + TBL_CMD_HISTORY, null);
		int maxID = 0;
		if (c != null && c.moveToFirst()) {
			maxID = c.getInt(0);
		}
		c.close();
		return maxID;
	}

	private int getMinCmdID() {
		Cursor c = dbInst.rawQuery("select min(" + DBName.history_id.name()
				+ ") from " + TBL_CMD_HISTORY, null);
		int minID = 0;
		if (c != null && c.moveToFirst()) {
			minID = c.getInt(0);
		}
		c.close();
		return minID;
	}

	/**
	 * Clear history with specified id and less
	 * 
	 * @param idAndLess
	 *            -- 0 if to clear all
	 */
	public void clearHistory(int idAndLess) {
		if (idAndLess < 0) {
			// do nothing
		} else if (idAndLess == 0) {
			dbInst.delete(TBL_CMD_HISTORY, null, null);
		} else {
			dbInst.delete(TBL_CMD_HISTORY, DBName.history_id.name() + "<="
					+ idAndLess, null);
			int minID = getMinCmdID();
			if (minID > 1) {
				// update remaining IDs to start from 1
				int offset = minID - 1;
				dbInst.execSQL("update " + TBL_CMD_HISTORY + " set "
						+ DBName.history_id.name() + " = "
						+ DBName.history_id.name() + " - " + offset);
			}
		}
	}
}
