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

import android.os.Handler;
import android.util.Log;

import java.lang.Thread;
import java.util.concurrent.Semaphore;
import java.util.LinkedHashMap;
import java.util.Iterator;
import java.util.Map;

import com.kufpg.androidhermit.R;

import de.sss.cmdconsole.builtincmd.CmdInfo;
import de.sss.cmdconsole.builtincmd.CmdSettings;
import de.sss.cmdconsole.builtincmd.FileSys;
import de.sss.cmdconsole.builtincmd.HelpCmd;
import de.sss.cmdconsole.builtincmd.SystemCmd;
import de.sss.cmdconsole.builtincmd.CmdSettings.Setting;
import de.sss.cmdconsole.common.*;

public class CommandDispatcher implements Runnable, IStdOut {
	private static final String CMDPROMPT = "#:-)> ";

	private Thread thrd;
	private Semaphore jobSem;
	private UiSideCommand uiSideCmd;

	private ApkRunner apkRunner;
	private CmdInfo cmdInfo;

	public CommandDispatcher(Handler uiHandler) {
		uiSideCmd = new UiSideCommand(uiHandler);

		// job semaphore
		jobSem = new Semaphore(0, true);

		thrd = new Thread(this);
		thrd.start(); // run() below is started after this statement
	}

	public void executeCommand(CmdInfo cmdInfo) {
		this.cmdInfo = cmdInfo;
		jobSem.release();
	}

	public boolean executeCommand(int historyID) {
		executeCommand(new CmdInfo(historyID));
		return true;
	}

	public boolean isApkRunning() {
		return (apkRunner != null);
	}

	/**
	 * Forward input to running apk
	 */
	public void write2ConStdIn(String s) {
		if (apkRunner != null) {
			writeln(s);
			apkRunner.write2ConStdIn(s);
		}
	}

	public void killRunningApk() {
		if (apkRunner != null) {
			apkRunner.interruptRunningApk();
		}
	}

	@Override
	public void run() {
		CmdSettings cmdSettings = new CmdSettings();
		FileSys fileSys = new FileSys(this);

		preCommandLoop(cmdSettings, fileSys);
		doCommandLoop(cmdSettings, fileSys);
	}

	private void waitForCommand() {
		boolean isWait = true;

		while (isWait) {
			try {
				jobSem.acquire();
				isWait = false;
			} catch (InterruptedException e) {

			}
		}
	}

	@Override
	public void write(String s) {
		uiSideCmd.write(s);
	}

	@Override
	public void writeln(String s) {
		uiSideCmd.writeln(s);
	}

	@Override
	public void printError(String s) {
		uiSideCmd.printError(s);
	}

	/**
	 * Init job prior to doCommandLoop
	 * 
	 * @param cmdSettings
	 * @param fileSys
	 */
	private void preCommandLoop(CmdSettings cmdSettings, FileSys fileSys) {
		boolean lsFullMode = cmdSettings.getBooleanVal(Setting.ls_full_mode);
		while (lsFullMode != fileSys.toggleListDirFullMode()) {
		} // toggle until same as saved value

		int fontSize = cmdSettings.getIntVal(Setting.console_font_size);
		uiSideCmd.setFontSize(fontSize);
	}

	private void doCommandLoop(CmdSettings cmdSettings, FileSys fileSys) {
		boolean isRun = true;

		HelpCmd.printHelpPrompt(this);

		while (isRun) {

			write(CMDPROMPT);

			waitForCommand();
			writeln(cmdInfo.getOrgCmd());

			CmdInfo.BuiltInCmd builtInCmd = this.cmdInfo.getBuiltInCmd();
			String[] args = cmdInfo.getCmdArgs();

			if (builtInCmd == CmdInfo.BuiltInCmd.ls) {
				if (args != null) {
					if (args.length >= 3) {
						HelpCmd.printUsage(this, builtInCmd);
					} else if (args.length == 2) {
						if (args[1].equalsIgnoreCase("-f")) {
							boolean isFullMode = fileSys
									.toggleListDirFullMode();
							cmdSettings.setValue(
									CmdSettings.Setting.ls_full_mode,
									isFullMode);
							writeln(CFunc
									.getString(isFullMode ? R.string.ls_fullmode_on
											: R.string.ls_fullmode_off));
						} else {
							String dirPath = FileSys.getResolvedPath(fileSys
									.getResultantPath(args[1]));
							if (!FileSys.isDir(dirPath)) {
								printError(dirPath
										+ CFunc.getString(R.string.error_not_dir));
							} else {
								fileSys.listDir(dirPath);
							}
						}
					} else {
						fileSys.listDir();
					}
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.pwd) {
				fileSys.printCurDir();
			} else if (builtInCmd == CmdInfo.BuiltInCmd.cd) {
				if (args != null && args.length == 2) {
					fileSys.changeDir(args[1]);
				} else if (args.length > 2) {
					HelpCmd.printUsage(this, builtInCmd);
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.clear) {
				uiSideCmd.clearScreen();
			} else if (builtInCmd == CmdInfo.BuiltInCmd.run) {
				if (args != null && args.length >= 2) {
					String apkPath = FileSys.getResolvedPath(fileSys
							.getResultantPath(args[1]));
					apkRunner = new ApkRunner(this, apkPath, args);
					apkRunner.execute();
					apkRunner = null;
				} else {
					HelpCmd.printUsage(this, builtInCmd);
				}

			} else if (builtInCmd == CmdInfo.BuiltInCmd.history) {
				LinkedHashMap<Integer, String> cmdHistoryList = cmdSettings
						.getHistoryList(0);
				if (cmdHistoryList != null && cmdHistoryList.size() > 0) {
					Iterator<Map.Entry<Integer, String>> it = cmdHistoryList
							.entrySet().iterator();
					while (it.hasNext()) {
						Map.Entry<Integer, String> entry = it.next();
						writeln(entry.getKey() + ": " + entry.getValue());
					}
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.del) {
				if (args != null && args.length == 2) {
					String filePath = FileSys.getResolvedPath(fileSys
							.getResultantPath(args[1]));
					if (!FileSys.deleteFile(filePath))
						printError(CFunc.getString(R.string.error_cant_del)
								+ filePath);
				} else {
					HelpCmd.printUsage(this, builtInCmd);
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.mkdir) {
				if (args != null && args.length == 2) {
					String dirPath = FileSys.getResolvedPath(fileSys
							.getResultantPath(args[1]));
					if (FileSys.fileExists(dirPath, false)) {
						printError(dirPath
								+ CFunc.getString(R.string.error_path_exists));
					} else {
						if (!FileSys.mkdir(dirPath))
							printError(CFunc
									.getString(R.string.error_create_dir)
									+ dirPath);
					}
				} else {
					HelpCmd.printUsage(this, builtInCmd);
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.ren) {
				if (args != null && args.length == 3) {
					String srcPath = FileSys.getResolvedPath(fileSys
							.getResultantPath(args[1]));
					String destPath = FileSys.getResolvedPath(fileSys
							.getResultantPath(args[2]));
					if (!FileSys.rename(srcPath, destPath))
						printError(String.format(
								CFunc.getString(R.string.error_cant_ren),
								srcPath, destPath));
				} else {
					HelpCmd.printUsage(this, builtInCmd);
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.cp) {
				if (args != null && args.length == 3) {
					String srcPath = FileSys.getResolvedPath(fileSys
							.getResultantPath(args[1]));
					String destPath = FileSys.getResolvedPath(fileSys
							.getResultantPath(args[2]));

					do {
						if (!FileSys.fileExists(srcPath, true)) {
							printError(srcPath
									+ CFunc.getString(R.string.error_notfile_notexist));
							break;
						}

						if (FileSys.isDir(destPath)) {
							String fileName = FileSys
									.getFileNameFromFullPath(srcPath);
							destPath = FileSys.combinePathAndFile(destPath,
									fileName);
						}

						if (!FileSys.copy(srcPath, destPath)) {
							printError(String.format(
									CFunc.getString(R.string.error_cant_cp),
									srcPath, destPath));
							break;
						}

					} while (false);
				} else {
					HelpCmd.printUsage(this, builtInCmd);
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.cleardex) {
				ApkLoader.deleteAllCachedDex();
			} else if (builtInCmd == CmdInfo.BuiltInCmd.help) {
				if (args != null) {
					if (args.length == 1)
						HelpCmd.printCommandMenu(this);
					else
						HelpCmd.printCommandDetail(this, args[1]);
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd._history_cmd_) {
				String cmdString = cmdSettings.getCommandById(cmdInfo
						.getHistoryID());
				if (cmdString != null) {
					CmdInfo newCmdInfo = CmdInfo.getCmdInfo(cmdString);
					executeCommand(newCmdInfo); // insert new command
				} else {
					printError(CFunc
							.getString(R.string.error_history_id_not_found));
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.ver) {
				writeln("");
				writeln(CFunc.getString(R.string.app_name) + " ["
						+ CFunc.getVersion() + "]");
				writeln("");
				writeln(CFunc.getContactMail());
				writeln("");
			} else if (builtInCmd == CmdInfo.BuiltInCmd.sres) {
				uiSideCmd.showScreenResolution();
			} else if (builtInCmd == CmdInfo.BuiltInCmd.netinfo) {
				SystemCmd.showNetworkInfo(this);
			} else if (builtInCmd == CmdInfo.BuiltInCmd.fontsize) {
				if (args != null) {
					if (args.length == 1) {
						uiSideCmd.showFontSize();
					} else {
						try {
							ConstantData.ConsoleFontSize fontSize = Enum
									.valueOf(
											ConstantData.ConsoleFontSize.class,
											args[1]);
							uiSideCmd.setFontSize(fontSize.v());
							cmdSettings.setValue(
									CmdSettings.Setting.console_font_size,
									fontSize.v());
						} catch (Exception e) {
							printError(CFunc
									.getString(R.string.error_unknown_fontsize));
							HelpCmd.printUsage(this, builtInCmd);
						}
					}
				}
			} else if (builtInCmd == CmdInfo.BuiltInCmd.exit) {
				isRun = false;
			} else if (builtInCmd == CmdInfo.BuiltInCmd._ui_exit_) {
				isRun = false;
			} else if (builtInCmd == CmdInfo.BuiltInCmd._unknown_) {
				if (!CFunc.isNullOrEmpty(cmdInfo.getOrgCmd()))
					printError(CFunc.getString(R.string.error_unknown_cmd)
							+ cmdInfo.getOrgCmd());
			}

			// save command history
			do {
				if (builtInCmd == CmdInfo.BuiltInCmd.history)
					break;

				if (CmdInfo.isHiddenCmd(builtInCmd))
					break;

				cmdSettings.saveCommand(this.cmdInfo.getOrgCmd());

			} while (false);

		} // end of while

		if (this.cmdInfo.getBuiltInCmd() == CmdInfo.BuiltInCmd.exit)
			uiSideCmd.exitApp();

		cmdSettings.close();

		Log.d("CommandDispatcher", "***** CommandDispatcher ends ****");
	}

}
