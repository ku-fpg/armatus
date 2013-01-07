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

import de.sss.cmdconsole.builtincmd.CmdInfo.BuiltInCmd;
import de.sss.cmdconsole.common.CFunc;
import de.sss.cmdconsole.common.IStdOut;

import java.lang.reflect.Field;

import com.kufpg.androidhermit.R;

public class HelpCmd {
	private HelpCmd() {
	}

	public static void printHelpPrompt(IStdOut stdOut) {
		stdOut.writeln(CFunc.getString(R.string.help_prompt));
	}

	public static void printCommandMenu(IStdOut stdOut) {
		stdOut.writeln(CFunc.getString(R.string.help_commands));
		stdOut.writeln("");

		BuiltInCmd[] cmdList = BuiltInCmd.values();
		for (BuiltInCmd c : cmdList) {
			if (!CmdInfo.isHiddenCmd(c))
				stdOut.writeln(c.name());
		}

		stdOut.writeln("");
		stdOut.writeln(CFunc.getString(R.string.cmd_help));
	}

	public static void printCommandDetail(IStdOut stdOut, String cmd) {
		try {
			Field field = R.string.class.getField("cmd_" + cmd);
			int value = field.getInt(null);
			stdOut.writeln(CFunc.getString(value));
		} catch (Exception e) {
			stdOut.writeln(CFunc.getString(R.string.cmd_unknown));
		}
	}

	public static void printUsage(IStdOut stdOut, CmdInfo.BuiltInCmd cmd) {
		try {
			Field field = R.string.class.getField("usage_" + cmd.name());
			int value = field.getInt(null);
			stdOut.writeln(CFunc.getString(value));
		} catch (Exception e) {
			stdOut.writeln(CFunc.getString(R.string.cmd_unknown));
		}
	}
}
