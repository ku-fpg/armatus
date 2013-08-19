package edu.kufpg.armatus.networking;

public interface HermitClient {
	void init();
	void doCommand();
	void getCommands();
	void reset();
}
