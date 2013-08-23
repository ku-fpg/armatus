package edu.kufpg.armatus.networking;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import edu.kufpg.armatus.BaseActivity;
import edu.kufpg.armatus.PrefsActivity;
import edu.kufpg.armatus.command.CommandDispatcher;
import edu.kufpg.armatus.console.ConsoleActivity;
import edu.kufpg.armatus.util.StringUtils;

public class HermitClient {

    private Token token;
    private ConsoleActivity mConsole;
    private enum RequestName {CONNECT, COMMAND, COMMANDS};
    private RequestName mRequestName;
    private HermitHttpGetRequest<JSONObject> connectRequest;
    private HermitHttpPostRequest<CommandResponse> commandResponse;
    private HermitHttpPostRequest<List<CommandInfo>> commandsResponse;
    
    public HermitClient(ConsoleActivity console) 
    {
    	mConsole = console;
    	connectRequest = new HermitHttpGetRequest<JSONObject>(mConsole){

			@Override
			protected JSONObject onResponse(String response) {
				// TODO Auto-generated method stub
				
				try {
					return new JSONObject(response);
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					return null;
				}
			}
    		
    	};
    	commandResponse = new HermitHttpPostRequest<CommandResponse>(mConsole){

			@Override
			protected CommandResponse onResponse(String response) {
				// TODO Auto-generated method stub
				
				try {
					return new CommandResponse(new JSONObject(response));
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
					return null;
				}
			}
    		
    	};
    	commandsResponse = new HermitHttpPostRequest<List<CommandInfo>>(mConsole){

			@Override
			protected List<CommandInfo> onResponse(String response) {
				// TODO Auto-generated method stub
				JSONObject insertNameHere = null;
				try {
					insertNameHere = new JSONObject(response);
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				if(insertNameHere == null)
				{
					return null;
				}
				else
				{
					
					try {
						String name = insertNameHere.getString("name");
						String help = insertNameHere.getString("help");
						JSONArray buba = insertNameHere.getJSONArray("tags");
						String[] tags = new String[buba.length()];
						for(int i = 0; i < buba.length(); i++)
						{
							tags[i] = buba.getString(i);
						}
						
						return Arrays.asList(new CommandInfo(name, help, tags));
					} catch (JSONException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
					
				}
			}
    		
    	};
    }
    
    private boolean isConnected(ConsoleActivity console, RequestName name)
    {
    	String server = PrefsActivity.getPrefs(console).getString(BaseActivity.NETWORK_SOURCE_KEY, null);
		if (BaseActivity.NETWORK_SOURCE_BLUETOOTH_SERVER.equals(server)) {
			if (BluetoothUtils.isBluetoothEnabled(console)) {
				if (BluetoothUtils.getBluetoothDevice(console) != null) {
					return true;
				} else {
					notifyDelay(name);
					BluetoothUtils.findDeviceName(console);
				}
			} else {
				notifyDelay(name);
				BluetoothUtils.enableBluetooth(console);
			}
		} else if (BaseActivity.NETWORK_SOURCE_WEB_SERVER.equals(server)) {
			if (InternetUtils.isAirplaneModeOn(console)) {
				console.appendConsoleEntry("Error: Please disable airplane mode before attempting to connect.");
			} else if (!InternetUtils.isWifiConnected(console) && !InternetUtils.isMobileConnected(console)) {
				notifyDelay(name);
				InternetUtils.enableWifi(console);
			} else {
				return true;
			}
		}
		return false;
    }

    public void connect() {
        // remember the server to talk to,
        // and initialize the class-specific token.
    	if(isConnected(mConsole, RequestName.CONNECT))
    	{
	        try {
	                String result = connection.post("/connect","");
	                JSONObject jsonToken = new JSONObject(result);
	                
	                token = new Token(jsonToken);
	                this.connection = connection;
	        } catch (Exception e) {
	                throw new Error("something bad has happened");
	        }
    	}
    }

    // Q: can two command-calls be in flight at the same time?
    // A: not for now, synchronized uses a lock for this
    public void command(String str) {
    	String[] words = str.split(StringUtils.WHITESPACE);
    	if(CommandDispatcher.isCustomCommand(words[0]))
    	{
    		CommandDispatcher.runOnConsole(mConsole, words[0], Arrays.copyOfRange(words, 1, words.length));
    	}
    	else
    	{
    		if(isConnected(mConsole, RequestName.COMMAND))
    		{
    			JSONObject o = new JSONObject();
                try {
        			o.put("token",token.toJSONObject());
        			o.put("cmd",str);
        		} catch (JSONException e1) {
        			// TODO Auto-generated catch block
        			e1.printStackTrace();
        		}
                
                try {
                        String result = connection.post("/command",o.toString());
                        //return new HermitClient.CommandResponse(new JSONObject(result));
                } catch (Exception e) {
                        throw new Error("something bad has happened");                
                }
    		}
    		
    	}
    }

    public void commands() {
    	if(isConnected(mConsole, RequestName.COMMANDS))
    	{
    		
    	}
    }

    public class CommandInfo {
        public final String name, help;
        public final String[] tags;

        public CommandInfo(String name, String help, String[] tags) {
            this.name = name;
            this.help = help;
            this.tags = tags;
        }

    }

    public static List<Glyph> listOfGlyphs(JSONArray a) {
            List<Glyph> list = new ArrayList<Glyph>();
            for(int i = 0;i < a.length();i++) {
                    try {
						list.add(new Glyph(a.getJSONObject(i)));
					} catch (JSONException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
            }
            return list;
    }

    public static class CommandResponse {
        public final Token token;
        public final List<Glyph> glyphs;

        public CommandResponse(JSONObject o) throws JSONException {
                this(new Token(o.getJSONObject("token")),listOfGlyphs(o.getJSONArray("glyphs")));
        }

        public CommandResponse(Token token,List<Glyph> glyphs) {
            this.token = token;
            this.glyphs = glyphs;
        }
    }

    public static enum GlyphStyle { 
        NORMAL, KEYWORD, SYNTAX, VAR, TYPE, LIT
    }

    
    public static GlyphStyle getStyle(JSONObject o) {
        if (o.has("style")) {
        	GlyphStyle mGlyphStyle = null;
                try {
					mGlyphStyle = GlyphStyle.valueOf(o.getString("style"));
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
                
                return mGlyphStyle;
        } else {
                return GlyphStyle.NORMAL;
        }
        
    }

    public static class Glyph {
        public final String text;
        public final GlyphStyle style;


        public Glyph(JSONObject o) throws JSONException {
                this(o.getString("text"),getStyle(o));
        }

        public Glyph(String text,GlyphStyle style) {
            this.text = text;
            this.style = style;
        }

    }

    public static class Token {
        public final int unique, token;

        public Token(JSONObject o) throws JSONException {
                this(o.getInt("unique"),o.getInt("token"));
        }

        public Token(int unique, int token) {
            this.unique = unique;
            this.token = token;
        }

        public JSONObject toJSONObject() {
                JSONObject o = new JSONObject();
                try {
					o.put("unique",unique);
					o.put("token",token);
				} catch (JSONException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
                
                return o;
        }
    }

    /*public static abstract class Continuation<V> {
        public abstract void call(V o);
    }*/

    public static abstract class Connection {
        public abstract void post(String url,String json);
        public abstract void get(String url, String json);
    }
    
    public void runDelayedMethod(RequestName name)
    {
    	switch(name)
    	{
	    	case CONNECT:
	    		connect();
	    		break;
	    	case COMMAND:
	    		commands();
	    		break;
	    	case COMMANDS:
	    		commands();
	    		break;
    	}
    }
    
    public void notifyDelay(RequestName name)
    {
    	mRequestName = name;
    }
    
    public boolean isDelayed()
    {
    	return mRequestName != null;
    }

}
