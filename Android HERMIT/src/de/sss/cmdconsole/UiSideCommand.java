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

import com.kufpg.androidhermit.R;

import de.sss.cmdconsole.common.CFunc;
import de.sss.cmdconsole.common.ConstantData;

import android.os.Handler;
import android.os.Message;

public class UiSideCommand {
	private Handler uiHandler;

	public UiSideCommand(Handler uiHandler) {
		this.uiHandler = uiHandler;
	}

	public void sendToUi(ConstantData.MsgType cmd, Object arg) {
		Message msg = Message.obtain();
		msg.what = cmd.ordinal();
		msg.setTarget(uiHandler);
		msg.obj = arg;
		msg.sendToTarget();
	}

	private void sendToUi(ConstantData.MsgType cmd) {
		sendToUi(cmd, null);
	}

	public void write(String s) {
		sendToUi(ConstantData.MsgType.STRING, s);
	}

	public void writeln(String s) {
		sendToUi(ConstantData.MsgType.STRING, s + ConstantData.NEWLINE);
	}

	public void printError(String s) {
		writeln(CFunc.getString(R.string.error_prefix) + s);
	}

	public void showScreenResolution() {
		sendToUi(ConstantData.MsgType.SHOWSRES);
	}

	public void clearScreen() {
		sendToUi(ConstantData.MsgType.CLEAR);
	}

	public void exitApp() {
		sendToUi(ConstantData.MsgType.EXIT);
	}

	public void setFontSize(int fontSize) {
		sendToUi(ConstantData.MsgType.SETFONTSIZE, fontSize);
	}

	public void showFontSize() {
		sendToUi(ConstantData.MsgType.SHOWFONTSIZE);
	}
}
