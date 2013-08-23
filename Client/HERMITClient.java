import java.util.List;
import java.util.LinkedList;

import org.json.JSONObject; // This is part of the Android API
import org.json.JSONArray; // This is part of the Android API

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

        String result = connection.post("/connect","");
        JSONObject jsonToken = new JSONObject(result);

        HERMITClient client = new HERMITClient();
        client.token = null;
        client.connection = connection;

        return client;
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

    public static List<Glyph> listOfGlyphs(JSONArray a) {
            java.util.LinkedList<Glyph> list = new java.util.LinkedList<Glyph>();
            for(int i = 0;i < a.length();i++) {
                    list.add(new Glyph(a.getJSONObject(i)));
            }
            return list;
    }

    public static class CommandResponse {
        public final Token token;
        public final List<Glyph> glyphs;

        public CommandResponse(JSONObject o) {
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
                return GlyphStyle.valueOf(o.getString("style"));
        } else {
                return GlyphStyle.NORMAL;
        }
    }

    public static class Glyph {
        public final String text;
        public final GlyphStyle style;


        public Glyph(JSONObject o) {
                this(o.getString("text"),getStyle(o));
        }

        public Glyph(String text,GlyphStyle style) {
            this.text = text;
            this.style = style;
        }

    }

    public static class Token {
        public final int unique, token;

        public Token(JSONObject o) {
                this(o.getInt("unique"),o.getInt("token"));
        }

        public Token(int unique, int token) {
            this.unique = unique;
            this.token = token;
        }

        public JSONObject toJSONObject() {
                JSONObject o = new JSONObject();
                o.put("unique",unique);
                o.put("token",token);
                return o;
        }
    }

    public static abstract class Continuation<V> {
        public abstract void call(V o);
    }

    public static abstract class Connection {
        public abstract String post(String url,String json);
    }

}
