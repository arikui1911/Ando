module ando.parser;

import std.string;
import std.array;
import std.ascii;
import ando.readstream;

import std.stdio;


class ParseError : Exception {
    static ParseError create(string msg, string file, int line, string dfile = __FILE__, int dline = __LINE__){
        auto e = new ParseError("%s:%d: %s".format(file, line, msg), dfile, dline);
        e.assign_location(file, line);
        return e;
    }

    this (string msg, string file = __FILE__, int line = __LINE__){
        super(msg, file, line);
    }

    private string file_;
    private int line_;

    private void assign_location(string file, int line){
        file_ = file;
        line_ = line;
    }

    @property string file(){ return file_; }
    @property int line(){ return line_; }
}

private enum TT {				// TokenType
    EOF,
    INTEGER,
    FLOATING,
    STRING,
    IDENT,

    IF,
    ELSIF,
    ELSE,
    UNLESS,
    WHILE,
    UNTIL,
    BREAK,
    CONTINUE,
    RETURN,
    DEF,
    TRUE,
    FALSE,
    NIL,

    EQ,
    NE,
    GE,
    LE,
    GT,
    LT,
    ASSIGN,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    ADD_A,
    SUB_A,
    MUL_A,
    DIV_A,
    MOD_A,

    DOT,
    COMMA,
    COLON,
    ARROW,
    SEMICOLON,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    LBRACKET,
    RBRACKET,
    DQUOTE,
    SQUOTE,
    REST,
}


private static TT[string] keywords;

static this() {
    keywords["if"]       = TT.IF;
    keywords["elsif"]    = TT.ELSIF;
    keywords["else"]     = TT.ELSE;
    keywords["unless"]   = TT.UNLESS;
    keywords["while"]    = TT.WHILE;
    keywords["until"]    = TT.UNTIL;
    keywords["break"]    = TT.BREAK;
    keywords["continue"] = TT.CONTINUE;
    keywords["return"]   = TT.RETURN;
    keywords["def"]      = TT.DEF;
    keywords["true"]     = TT.TRUE;
    keywords["false"]    = TT.FALSE;
    keywords["nil"]      = TT.NIL;

    keywords["=="] = TT.EQ;
    keywords["!="] = TT.NE;
    keywords[">="] = TT.GE;
    keywords["<="] = TT.LE;
    keywords[">"]  = TT.GT;
    keywords["<"]  = TT.LT;
    keywords["="]  = TT.ASSIGN;
    keywords["+"]  = TT.ADD;
    keywords["-"]  = TT.SUB;
    keywords["*"]  = TT.MUL;
    keywords["/"]  = TT.DIV;
    keywords["%"]  = TT.MOD;
    keywords["+="] = TT.ADD_A;
    keywords["-="] = TT.SUB_A;
    keywords["*="] = TT.MUL_A;
    keywords["/="] = TT.DIV_A;
    keywords["%="] = TT.MOD_A;

    keywords["."] = TT.DOT;
    keywords[","] = TT.COMMA;
    keywords[";"] = TT.SEMICOLON;
    keywords[":"] = TT.COLON;
    keywords["->"] = TT.ARROW;
    keywords["("] = TT.LPAREN;
    keywords[")"] = TT.RPAREN;
    keywords["{"] = TT.LBRACE;
    keywords["}"] = TT.RBRACE;
    keywords["["] = TT.LBRACKET;
    keywords["]"] = TT.RBRACKET;
    keywords["\""] = TT.DQUOTE;
    keywords["'"] = TT.SQUOTE;
    keywords["..."] = TT.REST;
}

private struct Token {
    TT tt;
    uint line;
    Appender!string buf;

    @property string value(){ return buf.data; }

	void put(char c){
		this.buf.put(c);
	}
	
	void pop_back(){
		auto tmp = buf.data.dup;
		tmp.popBack();
		buf = Appender!string(tmp);
	}
}

private enum LexState {
    INITIAL,
    COMMENT,
    NUMBER,
    FLOATING,
    STRING,
    STRESC,
    IDENT,
    OPERATOR,
}

class Lexer {
    private ReadStream src;
    private string file_;
    private uint line_;

    this (ReadStream source, string filename, uint lineno = 1){
        src = source;
        file_ = filename;
        line_ = lineno;
    }

    @property string file(){ return file_; }
    @property uint line(){ return line_; }

    private char getch() {
        auto c = src.getch();
        if (c == '\n') line_++;
        return c;
    }

    private void ungetch(char c) {
        if (c == '\n') line_--;
        src.ungetch(c);
    }

    private LexState lex_initial(Token *t, char c){
        if (c.isWhite()) {
            return LexState.INITIAL;
        } else if (c == '#') {
            return LexState.COMMENT;
        } else if (c == '"') {
            t.line = this.line;
            t.tt   = TT.STRING;
            return LexState.STRING;
        }

        t.put(c);
        t.line = this.line;
        if (c.isDigit()) {
            t.tt = TT.INTEGER;
            return LexState.NUMBER;
        } else if (c.isAlpha()) {
            t.tt = TT.IDENT;
            return LexState.IDENT;
        } else if (t.value in keywords) {
            return LexState.OPERATOR;
        } else {
            ungetch(c);
            throw ParseError.create("invalid character - `%c'".format(c), this.file, this.line);
            return LexState.INITIAL; // dont reach
        }
    }

    Token read(){
        Token t;
        char c;
        auto state = LexState.INITIAL;
        t.tt = TT.EOF;

    LEX: for (;;) {
            c = getch();
            final switch (state) {
            case LexState.INITIAL:
                state = lex_initial(&t, c);
                break;
            case LexState.COMMENT:
                if (c == '\n') state = LexState.INITIAL;
                break;
            case LexState.STRING:
                switch (c) {
                case '\\':
                    state = LexState.STRESC;
                case '"':
                    state = LexState.INITIAL;
                    break LEX;
                default:
                    t.put(c);
                }
                break;
            case LexState.STRESC:
                switch (c) {
                case '\\':
                case '"':
                    t.put(c);
                    break;
                case 'r':
                    t.put('\r');
                    break;
                case 'n':
                    t.put('\n');
                    break;
                case 't':
                    t.put('\t');
                    break;
                default:
                    t.put('\\');
                    t.put(c);
                }
                state = LexState.STRING;
                break;
            case LexState.NUMBER:
                if (c.isDigit()) {
                    t.put(c);
                } else if (c == '.') {
                    t.put(c);
                    t.tt  = TT.FLOATING;
                    state = LexState.FLOATING;
                } else {
                    ungetch(c);
                    break LEX;
                }
                break;
            case LexState.FLOATING:
                if (c.isDigit()) {
                    t.put(c);
                } else {
                    ungetch(c);
                    break LEX;
                }
                break;
            case LexState.IDENT:
                if (c.isAlphaNum()) {
                    t.put(c);
                } else {
                    ungetch(c);
                    break LEX;
                }
                break;
            case LexState.OPERATOR:
                t.put(c);
                if (t.value !in keywords) {
                    ungetch(c);
                    t.pop_back();
                    break LEX;
                }
                break;
            }
        }

        switch (state) {
        case LexState.STRING:
        case LexState.STRESC:
            throw ParseError.create("unterminated string literal", this.file, t.line);
            break;
        case LexState.IDENT:
            if (t.value in keywords) {
                t.tt = keywords[t.value];
            }
            break;
        case LexState.OPERATOR:
            t.tt = keywords[t.value];
            break;
        default:
            break;
        }

        return t;
    }

    unittest {
        auto s = new StringReadStream("123.45");
        auto lexer = new Lexer(s, "(test)");

        auto t = lexer.read();
        assert(t.tt == TT.FLOATING);
        assert(t.value == "123.45");
    }
}


