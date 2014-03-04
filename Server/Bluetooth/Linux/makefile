CODE_DIR=WinBluetoothServer

all: server_ex

# server_ex: bluez_server.o
	# ghc -Wall -I/usr/include/glib-2.0/ -I/usr/lib/glib-2.0/include --make -main-is ServerEx -o server_ex ServerEx.hs bluez_server.o -lbluetooth

server_ex: win_bluetooth_server.o
	ghc -Wall --make -main-is ServerEx -o server_ex ServerEx.hs win_bluetooth_server.o -lbluetooth

# bluez_server.o: bluez_server.c bluez_server.h
	# gcc -c -g -Wall bluez_server.c

win_bluetooth_server.o: $(CODE_DIR)/win_bluetooth_server.c $(CODE_DIR)/win_bluetooth_server.h
	msbuild $(CODE_DIR).sln

clean:
	rm -f *.o *.hi *.chi *.i *.chs.h server_ex