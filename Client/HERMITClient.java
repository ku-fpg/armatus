import java.util.List;

public class HERMITClient {

    public HERMITClient() {}

    public Token connect(String url) {
	return null;
    }

    public CommandResponse command(Token token, String str) {
	return null;
    }

    public List<CommandInfo> commands(Token token) {
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

}
