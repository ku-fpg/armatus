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
import de.sss.cmdconsole.common.IStdOut;
import android.content.Context;
import android.net.wifi.WifiManager;
import android.net.wifi.WifiInfo;
import android.net.DhcpInfo;

import java.net.NetworkInterface;
import java.net.InetAddress;
import java.util.Enumeration;

import com.kufpg.androidhermit.R;

public class SystemCmd {
	private SystemCmd() {
	}

	public static void showNetworkInfo(IStdOut stdOut) {
		WifiManager wifiManager = (WifiManager) CFunc.getAppInst()
				.getSystemService(Context.WIFI_SERVICE);
		if (wifiManager != null && wifiManager.isWifiEnabled()
				&& wifiManager.getWifiState() == WifiManager.WIFI_STATE_ENABLED) {
			// WiFi enabled
			WifiInfo wInfo = wifiManager.getConnectionInfo();
			DhcpInfo dInfo = wifiManager.getDhcpInfo();

			stdOut.writeln(CFunc.getString(R.string.netinfo_wifiinfo));

			stdOut.write(CFunc.getString(R.string.netinfo_macaddr) + ": ");
			stdOut.writeln(wInfo.getMacAddress());
			stdOut.write(CFunc.getString(R.string.netinfo_ssid) + ": ");
			stdOut.writeln(wInfo.getSSID());

			stdOut.write(CFunc.getString(R.string.netinfo_ipaddr) + ": ");
			stdOut.writeln(CFunc.int2Ipv4(dInfo.ipAddress));
			stdOut.write(CFunc.getString(R.string.netinfo_netmask) + ": ");
			stdOut.writeln(CFunc.int2Ipv4(dInfo.netmask));
			stdOut.write(CFunc.getString(R.string.netinfo_gateway) + ": ");
			stdOut.writeln(CFunc.int2Ipv4(dInfo.gateway));
			stdOut.write(CFunc.getString(R.string.netinfo_dns1) + ": ");
			stdOut.writeln(CFunc.int2Ipv4(dInfo.dns1));
			stdOut.write(CFunc.getString(R.string.netinfo_dns2) + ": ");
			stdOut.writeln(CFunc.int2Ipv4(dInfo.dns2));
		} else {
			// may be using 3G network

			try {
				Enumeration<NetworkInterface> netList = NetworkInterface
						.getNetworkInterfaces();
				while (netList.hasMoreElements()) {
					NetworkInterface intf = netList.nextElement();

					stdOut.write(CFunc.getString(R.string.netinfo_netintf_name)
							+ ": ");
					stdOut.writeln(intf.getDisplayName());

					Enumeration<InetAddress> addrList = intf.getInetAddresses();
					while (addrList.hasMoreElements()) {
						InetAddress inetAddress = addrList.nextElement();
						// if (!inetAddress.isLoopbackAddress()) {
						stdOut.write(CFunc.getString(R.string.netinfo_ipaddr)
								+ ": ");
						stdOut.writeln(CFunc.int2Ipv4(inetAddress.getAddress()));
						// }
					}
				}
			} catch (Exception e) {
				stdOut.writeln(e.getClass().getName() + ": " + e.getMessage());
			}

		}
	}

}
