package edu.kufpg.armatus.server;

import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.console.GetCommandsRequest;

public class HermitWebClient extends AbsHermitClient {

	public HermitWebClient(ConsoleActivity console) {
		super(console);
	}

	@Override
	public void init() {
		new HermitWebServerRequest<String>(getConsole()) {
			@Override
			protected String doInBackground(String... params) {
				return null;
			}
		}.execute("INIT");
	}

	@Override
	public void doCommand() {
	}

	@Override
	public void getCommands() {
		new GetCommandsRequest(getConsole()).execute("GET ME SOME COMMANDS");
	}

	@Override
	public void reset() {
		new HermitWebServerRequest<String>(getConsole()) {
			@Override
			protected String doInBackground(String... params) {
				return null;
			}
		}.execute("RESET");
	}

}
