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

import de.sss.cmdconsole.common.CFunc;

/**
 * A class to contain command information
 */
public class CmdInfo {
	// ! these are built-in commands
	public enum BuiltInCmd {
		_unknown_, // hidden command starts and ends with "_"
		_ui_exit_, _history_cmd_, help, ls, pwd, cd, clear, consider, run, history, del, mkdir, ren, resume, cp, cleardex, ver, sres, netinfo, fontsize, exit,
	};

	public static final String HISTORYCMD_PREFIX = "!";

	private BuiltInCmd builtInCmd;
	private String orgCmdString;
	private String[] cmdArgs;
	private boolean isToBeFetchedFromHistory;

	public CmdInfo(BuiltInCmd builtInCmd, String orgCmdString, String[] cmdArgs) {
		this.builtInCmd = builtInCmd;
		this.orgCmdString = orgCmdString;
		this.cmdArgs = cmdArgs;
	}

	public CmdInfo(int historyID) {
		this.builtInCmd = BuiltInCmd._history_cmd_;
		this.orgCmdString = CmdInfo.HISTORYCMD_PREFIX
				+ String.valueOf(historyID);
		this.isToBeFetchedFromHistory = true;
	}

	public BuiltInCmd getBuiltInCmd() {
		return builtInCmd;
	}

	public String getOrgCmd() {
		return orgCmdString;
	}

	public String[] getCmdArgs() {
		return cmdArgs;
	}

	public boolean isToBeFetchedFromHistory() {
		return isToBeFetchedFromHistory;
	}

	public int getHistoryID() {
		return CFunc.parseInt(orgCmdString.substring(1));
	}

	/**
	 * Generate a CmdInfo from a command line string
	 * 
	 * @param cmdString
	 * @return
	 */
	public static CmdInfo getCmdInfo(String cmdString) {
		String[] args = cmdString.split(" ");
		if (args != null && args.length > 0) {
			try {
				CmdInfo.BuiltInCmd builtInCmd = Enum.valueOf(
						CmdInfo.BuiltInCmd.class, args[0]);
				CmdInfo cmdInfo = new CmdInfo(builtInCmd, cmdString, args);
				return cmdInfo;
			} catch (Exception e) {

			}
		}
		return null;
	}

	public static boolean isHiddenCmd(BuiltInCmd cmd) {
		String cmdName = cmd.name();
		if (cmdName.startsWith("_") || cmdName.endsWith("_"))
			return true;
		return false;
	}
}
