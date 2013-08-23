import java.util.List;
import org.json.JSONObject; // This is part of the Android API

public class HERMITClient {

    private HERMITClient() {}

    private Token token;
    private Connection connection;

    /*
     * This version of post take a JSONObject,
     * and returns a JSONObject. 
     *
     */
    private JSONObject post(String url,JSONObject arg) {
	String result = connection.post(url,arg.toString());
	return new JSONObject(result);
    }

    public static HERMITClient connect(Connection connection) {
	// remember the server to talk to,
	// and initialize the class-specific token.

	connection.post("/connect","");

	return null;
    }

    // Q: can two command-calls be in flight at the same time?
    public CommandResponse command(String str) {
	return null;
    }

    public List<CommandInfo> commands() {
	return null;
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

    public class CommandResponse {
	public final Token token;
	public final List<Glyph> glyphs;

	public CommandResponse(Token token,List<Glyph> glyphs) {
	    this.token = token;
	    this.glyphs = glyphs;
	}


    }

    public enum GlyphStyle { 
	NORMAL, KEYWORD, SYNTAX, VAR, TYPE, LIT
    }

    public class Glyph {
	public final String text;
	public final GlyphStyle style;

	public Glyph(String text,GlyphStyle style) {
	    this.text = text;
	    this.style = style;
	}
    }

    public class Token {
	public final int unique, token;

	public Token(int unique, int token) {
	    this.unique = unique;
	    this.token = token;
	}
    }

    public abstract class Continuation<V> {
	public abstract void call(V o);
    }

    public abstract class Connection {
	public abstract String post(String url,String json);
    }

}
