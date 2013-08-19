package edu.kufpg.armatus.command;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.PrefsActivity;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.networking.BluetoothUtils;
import edu.kufpg.armatus.networking.InternetUtils;

public abstract class ServerDefinedCommand extends Command {
	public ServerDefinedCommand(String commandName,	int argsCount) {
		super(commandName, argsCount);
	}

	public ServerDefinedCommand(String commandName,	int argsCount, boolean lowerArgBound) {
		super(commandName, argsCount, lowerArgBound);
	}
	
	@Override
	protected void run(ConsoleActivity console, String... args) {
		String server = PrefsActivity.getPrefs(console).getString(BaseActivity.NETWORK_SOURCE_KEY, null);
		if (BaseActivity.NETWORK_SOURCE_BLUETOOTH_SERVER.equals(server)) {
			if (BluetoothUtils.isBluetoothEnabled(console)) {
				if (BluetoothUtils.getBluetoothDevice(console) != null) {
					onConnect(console, args);
				} else {
					CommandDispatcher.delayCommand(this, false, args);
					BluetoothUtils.findDeviceName(console);
				}
			} else {
				CommandDispatcher.delayCommand(this, false, args);
				BluetoothUtils.enableBluetooth(console);
			}
		} else if (BaseActivity.NETWORK_SOURCE_WEB_SERVER.equals(server)) {
			if (InternetUtils.isAirplaneModeOn(console)) {
				console.appendConsoleEntry("Error: Please disable airplane mode before attempting to connect.");
			} else if (!InternetUtils.isWifiConnected(console) && !InternetUtils.isMobileConnected(console)) {
				CommandDispatcher.delayCommand(this, false, args);
				InternetUtils.enableWifi(console);
			} else {
				onConnect(console, args);
			}
		}
	}
	
	protected abstract void onConnect(ConsoleActivity console, String... args);

}