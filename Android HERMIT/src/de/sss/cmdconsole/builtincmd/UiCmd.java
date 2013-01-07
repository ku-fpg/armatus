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

import com.kufpg.androidhermit.R;

import de.sss.cmdconsole.common.*;

import android.graphics.Point;
import android.util.DisplayMetrics;
import android.view.Display;

public class UiCmd {
	private UiCmd() {
	}

	public static void showScreenResolution(Display display, IStdOut stdOut) {
		DisplayMetrics metrics = new DisplayMetrics();
		Point resolution = new Point();
		display.getMetrics(metrics);
		display.getSize(resolution);

		stdOut.writeln(CFunc.getString(R.string.display_height) + ": "
				+ resolution.y);
		stdOut.writeln(CFunc.getString(R.string.display_width) + ": "
				+ resolution.x);

		stdOut.writeln(CFunc.getString(R.string.metrics_density) + ": "
				+ metrics.density);
		stdOut.writeln(CFunc.getString(R.string.metrics_densityDpi) + ": "
				+ metrics.densityDpi);
		stdOut.writeln(CFunc.getString(R.string.metrics_scaledDensity) + ": "
				+ metrics.scaledDensity);
		stdOut.writeln(CFunc.getString(R.string.metrics_heightPixels) + ": "
				+ metrics.heightPixels);
		stdOut.writeln(CFunc.getString(R.string.metrics_widthPixels) + ": "
				+ metrics.widthPixels);
		stdOut.writeln(CFunc.getString(R.string.metrics_xdpi) + ": "
				+ metrics.xdpi);
		stdOut.writeln(CFunc.getString(R.string.metrics_ydpi) + ": "
				+ metrics.ydpi);
	}
}
