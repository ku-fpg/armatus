package edu.kufpg.armatus.server;

public interface HermitClient {
	void init();
	void doCommand();
	void getCommands();
	void reset();
}
