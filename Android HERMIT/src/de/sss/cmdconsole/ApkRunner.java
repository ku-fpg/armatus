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

import android.util.Log;

import java.lang.Thread;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.io.PrintStream;
import java.io.InputStreamReader;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.io.File;
import java.util.HashMap;

import de.sss.cmdconsole.common.CFunc;
import de.sss.cmdconsole.common.ConstantData;
import de.sss.cmdconsole.common.IStdOut;

public class ApkRunner implements Runnable {
	private static final int REQENDENTRY = 0xaeaeaeae;

	private Thread targetApkRunningThread;
	private Thread loaderRunningThread;

	private IStdOut stdOut;
	private String apkPath;
	private Method mainMethod;
	private String[] cmdArgs;
	private HashMap<Integer, Object> envArgs;

	private PipedOutputStream ui2ConOutStream;
	private PipedOutputStream con2UiOutStream;
	private PipedInputStream uiInStream;
	private PipedInputStream conInStream;

	private PrintStream wrappedUi2ConOutStream;
	private PrintStream wrappedCon2UiOutStream;

	private static void createApkCacheDir() {
		File f = new File(ApkLoader.getDexCacheDir());
		if (!f.exists()) {
			f.mkdir();
		}
	}

	public ApkRunner(IStdOut stdOut, String apkPath, String[] args) {
		this.stdOut = stdOut;
		this.apkPath = apkPath;

		if (args.length > 2) { // construct command line arguments if any
			this.cmdArgs = new String[args.length - 2];
			for (int i = 0; i < this.cmdArgs.length; ++i) {
				this.cmdArgs[i] = args[i + 2];
			}
		}

		createApkCacheDir();
	}

	public boolean execute() {

		try {

			mainMethod = ApkLoader.loadEntryPoint(apkPath);

			initPipes();

			// get an instance of current thread
			loaderRunningThread = Thread.currentThread();

			// start thread to run the apk
			targetApkRunningThread = new Thread(this);
			targetApkRunningThread.start();

			readStdOutFromApkProgram(); // read output from apk program until it
										// dies

		} catch (ClassNotFoundException e) {
			stdOut.writeln("Entry point class not found: " + e.getMessage());
			return false;
		} catch (NoSuchMethodException e) {
			stdOut.writeln("Entry point method not found: " + e.getMessage());
			return false;
		} catch (IOException e) {
			stdOut.writeln("I/O Error: " + e.getMessage());
			return false;
		} catch (Exception e) {
			stdOut.writeln("Unknnown error: " + e.getMessage());
			return false;
		}

		return true;
	}

	/**
	 * Init 2 pairs of pipe stream
	 * 
	 * @throws IOException
	 */
	private void initPipes() throws IOException {
		uiInStream = new PipedInputStream();
		con2UiOutStream = new PipedOutputStream(uiInStream);
		conInStream = new PipedInputStream();
		ui2ConOutStream = new PipedOutputStream(conInStream);

		wrappedUi2ConOutStream = new PrintStream(ui2ConOutStream, true,
				ConstantData.STREAMENCODING);
		wrappedCon2UiOutStream = new PrintStream(con2UiOutStream, true,
				ConstantData.STREAMENCODING);
	}

	/**
	 * Write to console stdIn
	 */
	public void write2ConStdIn(String s) {
		if (wrappedUi2ConOutStream != null)
			wrappedUi2ConOutStream.println(s);
	}

	public void interruptRunningApk() {
		envArgs.put(REQENDENTRY, true);
		targetApkRunningThread.interrupt();
	}

	/**
	 * Read data from stdout of apk program
	 */
	private void readStdOutFromApkProgram() throws UnsupportedEncodingException {
		final int BUFFERSIZE = 128;

		// init string buffers
		StringBuilder sbBuf = new StringBuilder(BUFFERSIZE);
		char[] charBuf = new char[BUFFERSIZE];

		// wrap input stream
		BufferedReader reader = new BufferedReader(new InputStreamReader(
				uiInStream, ConstantData.STREAMENCODING));

		boolean isInterrupted = false;
		int quitCount = 10;
		while (true) {

			try {
				if (quitCount < 0)
					break;

				if (isInterrupted) {
					--quitCount;
					if (!reader.ready()) {
						CFunc.sleep(20);
						continue;
					}
				}

				int nrRead = reader.read(charBuf);
				if (nrRead > 0) {

					sbBuf.delete(0, sbBuf.length());
					sbBuf.append(charBuf, 0, nrRead);
					this.stdOut.write(sbBuf.toString());
				}
			} catch (IOException e) {
				isInterrupted = true;
				Log.d("ApkRunner", "**** target running thread ends ****");
			}

			if (!targetApkRunningThread.isAlive())
				isInterrupted = true;

		} // end of while

		try {
			wrappedUi2ConOutStream.close();
			wrappedCon2UiOutStream.close();

			uiInStream.close();
			con2UiOutStream.close();
			conInStream.close();
			ui2ConOutStream.close();
		} catch (Exception e) {

		}

		Log.d("ApkRunner", "**** loader ends ****");
	}

	/**
	 * Thread to run the apk program
	 */
	@Override
	public void run() {
		try {
			// a static method, first argument should be null
			envArgs = new HashMap<Integer, Object>();
			envArgs.put(0, CFunc.getAppInst()); // application context
			envArgs.put(1, cmdArgs); // command line arguments
			envArgs.put(2, conInStream); // stdin
			envArgs.put(3, wrappedCon2UiOutStream); // stdout
			envArgs.put(4, ConstantData.STREAMENCODING); // stream encoding

			mainMethod.invoke(null, envArgs);

		} catch (Exception e) {
			CFunc.sleep(200);
			printException(e);
		}

		CFunc.sleep(200);

		Log.d("ApkRunner", "###### apk running thread ends #####");

		// so that loader thread can jump out of a blocking read
		loaderRunningThread.interrupt();

		// clear
		envArgs.clear();
		envArgs = null;
	}

	private void printException(Exception e) {
		StackTraceElement[] elementList = null;

		if (e instanceof InvocationTargetException) {
			InvocationTargetException invExp = (InvocationTargetException) e;
			Throwable th = invExp.getCause();
			if (th != null) {
				stdOut.write("Error: " + th.getMessage());
				stdOut.writeln(" (" + th.getClass().getName() + ")");
				elementList = th.getStackTrace();
			}
		}

		if (elementList == null) {
			stdOut.write("Error: " + e.getMessage());
			stdOut.writeln(" (" + e.getClass().getName() + ")");
			elementList = e.getStackTrace();
		}

		if (elementList != null) {
			for (StackTraceElement ele : elementList) {
				stdOut.writeln("\t" + ele.getClassName() + ele.getMethodName()
						+ "(" + ele.getLineNumber() + ")");
			}
		}
	}
}
