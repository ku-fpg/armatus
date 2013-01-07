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

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Date;

import com.kufpg.androidhermit.R;

import de.sss.cmdconsole.common.CFunc;
import de.sss.cmdconsole.common.ConstantData;
import de.sss.cmdconsole.common.IStdOut;

public class FileSys {
	private static final int WHITESPACE_FILESIZE = 8;
	private static String privateDataRoot;

	private File curDir;
	private IStdOut stdOut;
	private boolean isLSFullMode;
	private DecimalFormat decimalFormat = new DecimalFormat("#.##");
	private SimpleDateFormat dateFormat = new SimpleDateFormat(
			"yyyy-MM-dd HH:mm:ss");

	public FileSys(IStdOut stdOut) {
		curDir = new File(ConstantData.ROOTDIR); // points to root
		this.stdOut = stdOut;
		this.isLSFullMode = false;
	}

	public void listDir() {
		listDir(curDir);
	}

	public void listDir(String dirPath) {
		File d = new File(dirPath);
		listDir(d);
	}

	/**
	 * Toggle list dir mode
	 * 
	 * @return toggled flag
	 */
	public boolean toggleListDirFullMode() {
		isLSFullMode = !isLSFullMode;
		return isLSFullMode;
	}

	private String getPrintableSize(long fileSize) {
		if (fileSize < 1024) {
			return fileSize + "B";
		} else if (fileSize < 1048576) {
			double size = (double) fileSize / 1024;
			return decimalFormat.format(size) + "KB";
		}

		double size = (double) fileSize / 1048576;
		return decimalFormat.format(size) + "MB";
	}

	private String getPrintableDateTime(long lastModTime) {
		if (lastModTime > 0) {
			Date d = new Date();
			d.setTime(lastModTime);
			return dateFormat.format(d);
		}

		return "                   "; // return empty string
	}

	private String getWhiteSpace(int nrWhite) {
		switch (nrWhite) {
		case 1:
			return " ";
		case 2:
			return "  ";
		case 3:
			return "   ";
		case 4:
			return "    ";
		case 5:
			return "     ";
		case 6:
			return "      ";
		case 7:
			return "       ";
		case 8:
			return "        ";
		case 9:
			return "         ";
		}

		return " ";
	}

	private void printFileRow(String fileName, long lastModTime, long fileSize,
			boolean isDir) {
		// date time
		stdOut.write(getPrintableDateTime(lastModTime));
		stdOut.write("  ");

		// file size
		String fSizeStr = getPrintableSize(fileSize);
		if (fSizeStr.length() < WHITESPACE_FILESIZE)
			stdOut.write(getWhiteSpace(WHITESPACE_FILESIZE - fSizeStr.length()));
		stdOut.write(fSizeStr);
		stdOut.write("  ");

		// name
		if (isDir) {
			stdOut.write(fileName);
			stdOut.writeln(ConstantData.DIRSEPARATOR);
		} else {
			stdOut.writeln(fileName);
		}
	}

	private void printFileNameOnly(String fileName, boolean isDir) {
		if (isDir) {
			stdOut.write(fileName);
			stdOut.writeln(ConstantData.DIRSEPARATOR);
		} else {
			stdOut.writeln(fileName);
		}
	}

	public void listDir(File dirPath) {
		File[] list = dirPath.listFiles();
		if (list != null && list.length > 0) {
			for (File f : list) {
				if (isLSFullMode)
					printFileRow(f.getName(), f.lastModified(), f.length(),
							f.isDirectory());
				else
					printFileNameOnly(f.getName(), f.isDirectory());
			}
		}
	}

	public void printCurDir() {
		stdOut.writeln(curDir.getAbsolutePath());
	}

	public void changeDir(String dir) {
		do {

			File f = new File(getResultantPath(curDir, dir));
			if (!f.exists()) {
				stdOut.printError(CFunc.getString(R.string.error_cant_cd_dir)
						+ f.getPath());
				break;
			}

			if (!f.isDirectory()) {
				stdOut.printError(CFunc
						.getString(R.string.error_target_not_dir) + f.getPath());
				break;
			}

			if (!f.canRead()) {
				stdOut.printError(CFunc
						.getString(R.string.error_target_dir_cant_read)
						+ f.getPath());
				break;
			}

			curDir = new File(getResolvedPath(f.getAbsolutePath()));

		} while (false);
	}

	public static boolean fileExists(String path, boolean chkIsFile) {
		try {
			File f = new File(path);
			if (!f.exists())
				return false;

			if (chkIsFile) {
				if (!f.isFile())
					return false;
			}
			return true;
		} catch (Exception e) {

		}
		return false;
	}

	public static boolean deleteFile(String path) {
		try {
			File f = new File(path);
			return f.delete();
		} catch (Exception e) {

		}

		return false;
	}

	public static boolean mkdir(String path) {
		try {
			File f = new File(path);
			return f.mkdir();
		} catch (Exception e) {

		}

		return false;
	}

	public static boolean rename(String src, String dest) {
		try {
			File fSrc = new File(src);
			File fDest = new File(dest);
			return fSrc.renameTo(fDest);
		} catch (Exception e) {

		}

		return false;
	}

	public static boolean isFile(String path) {
		try {
			File f = new File(path);
			return f.isFile();
		} catch (Exception e) {

		}

		return false;
	}

	public static boolean isDir(String path) {
		try {
			File f = new File(path);
			return f.isDirectory();
		} catch (Exception e) {

		}

		return false;
	}

	public static boolean copy(String src, String dest) {
		FileOutputStream fOut = null;
		FileInputStream fIn = null;

		try {
			fOut = new FileOutputStream(dest);
			fIn = new FileInputStream(src);
			byte[] buf = new byte[256];
			while (true) {
				int nrRead = fIn.read(buf);
				if (nrRead <= 0)
					break;
				fOut.write(buf, 0, nrRead);
			}

			return true;
		} catch (Exception e) {
			return false;
		} finally {
			try {
				if (fOut != null)
					fOut.close();
			} catch (IOException e) {
			}
			try {
				if (fIn != null)
					fIn.close();
			} catch (IOException e) {
			}
		}

	}

	public static String getFileNameFromFullPath(String path) {
		int pos = path.lastIndexOf(ConstantData.DIRSEPARATOR);
		if (pos != -1) {
			if (pos == path.length() - 1)
				return null;
			return path.substring(pos + 1);
		}

		return path;
	}

	public static String combinePathAndFile(String path, String fileName) {
		if (!path.endsWith(ConstantData.DIRSEPARATOR))
			path += ConstantData.DIRSEPARATOR;

		path += fileName;
		return path;
	}

	public static String getPrivateDataRoot() {
		return privateDataRoot;
	}

	public static void setPrivateDataRoot(String dir) {
		privateDataRoot = dir;
	}

	public String getResultantPath(String path) {
		return getResultantPath(curDir, path);
	}

	public static String getResultantPath(File base, String path) {
		String result = null;
		if (path.startsWith(ConstantData.ROOTDIR)) {
			result = path;
		} else {
			result = base.getAbsolutePath();
			if (!result.endsWith(ConstantData.DIRSEPARATOR))
				result += ConstantData.DIRSEPARATOR;
			result += path;
		}

		return result;
	}

	private enum DirType {
		NONE, CURDIR, PARENTDIR,
	};

	/**
	 * Check if sb, starting from i, has a pattern of dot and slash or a single
	 * dot: './' or '.'
	 */
	private static boolean checkDotSlash(StringBuilder sb, int i) {
		char c = sb.charAt(i);
		if (c != '.')
			return false;

		if (i + 1 >= sb.length())
			return true;

		c = sb.charAt(i + 1);
		if (c != ConstantData.DIRSEPCHAR)
			return false;

		return true;
	}

	/**
	 * Remove "..", "." and redundant '/' in orgPath
	 * 
	 * The File class of Android cannot remove these characters using either
	 * getAbsolutePath() or getCanonicalPath(), thus, I have to write it myself
	 * here. (The normal J2SE version can achieve this using getAbsolutePath())
	 * 
	 * @param orgPath
	 *            -- path must starts with '/'
	 */
	public static String getResolvedPath(String orgPath) {
		if (orgPath == null || !orgPath.startsWith(ConstantData.ROOTDIR))
			return orgPath;

		StringBuilder sb = new StringBuilder(orgPath);
		int i = 0;
		while (i < sb.length()) {
			if (sb.charAt(i) == ConstantData.DIRSEPCHAR) {
				if (i + 1 >= sb.length()) {
					if (i != 0)
						sb.deleteCharAt(i); // delete trailing slash
					break;
				}

				++i;
				if (sb.charAt(i) == ConstantData.DIRSEPCHAR) { // repeated '/'
					sb.deleteCharAt(i);
					--i;
					continue;
				}

				// determine '.' or '..'
				DirType dt = DirType.NONE;
				if (checkDotSlash(sb, i)) {
					dt = DirType.CURDIR;
				} else {
					if (sb.charAt(i) == '.') {
						if (checkDotSlash(sb, i + 1))
							dt = DirType.PARENTDIR;
					}
				}

				// starting trimming
				if (dt == DirType.CURDIR) {
					int end = i + 2;
					end = end > sb.length() ? sb.length() : end;
					sb.delete(i, end);
					--i;
				} else if (dt == DirType.PARENTDIR) {
					int end = i + 3;
					end = end > sb.length() ? sb.length() : end;
					sb.delete(i, end);
					// clean previous dir, too
					if (i - 2 > 0) { // we haven't reached root, yet
						int start = sb.lastIndexOf("/", i - 2);
						sb.delete(start + 1, i);
						i = start;
					} else {
						--i;
					}
				}

			} else {
				++i;
			}
		}

		return sb.toString();
	}

}
