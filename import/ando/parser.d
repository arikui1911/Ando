module ando.parser;

import std.string;
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
    REST,
    BANG,
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
    keywords["..."] = TT.REST;
    keywords["!"] = TT.BANG;
}

private struct Token {
    import std.array;

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
    import std.ascii;

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
        } else if (in_keyword(t)) {
            return LexState.OPERATOR;
        } else {
            ungetch(c);
            throw ParseError.create("invalid character - `%c'".format(c), this.file, this.line);
            return LexState.INITIAL; // dont reach
        }
    }

    private bool in_keyword(Token *t){
        foreach (key; keywords.byKey) {
            if (key.indexOf(t.value) == 0) return true;
        }
        return false;
    }

    Token read(){
        Token t;
        char c;
        auto state = LexState.INITIAL;
        t.tt = TT.EOF;

    LEX: for (;;) {
            c = getch();
            if (src.is_eof) break;

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
                if (!in_keyword(&t)) {
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
        import core.exception;

        struct TestData {
            string src;
            TT     expected;
            string expect_val;
        }

        TestData[] test_data = [
                                {"123",      TT.INTEGER  },
                                {"123.45",   TT.FLOATING },
                                {"\"abc\"",  TT.STRING, "abc"},
                                {"abc",      TT.IDENT    },
                                {"if",       TT.IF       },
                                {"elsif",    TT.ELSIF    },
                                {"else",     TT.ELSE     },
                                {"unless",   TT.UNLESS   },
                                {"while",    TT.WHILE    },
                                {"until",    TT.UNTIL    },
                                {"break",    TT.BREAK    },
                                {"continue", TT.CONTINUE },
                                {"return",   TT.RETURN   },
                                {"def",      TT.DEF      },
                                {"true",     TT.TRUE     },
                                {"false",    TT.FALSE    },
                                {"nil",      TT.NIL      },
                                {"==",       TT.EQ       },
                                {"!=",       TT.NE       },
                                {">=",       TT.GE       },
                                {"<=",       TT.LE       },
                                {">",        TT.GT       },
                                {"<",        TT.LT       },
                                {"=",        TT.ASSIGN   },
                                {"+",        TT.ADD      },
                                {"-",        TT.SUB      },
                                {"*",        TT.MUL      },
                                {"/",        TT.DIV      },
                                {"%",        TT.MOD      },
                                {"+=",       TT.ADD_A    },
                                {"-=",       TT.SUB_A    },
                                {"*=",       TT.MUL_A    },
                                {"/=",       TT.DIV_A    },
                                {"%=",       TT.MOD_A    },
                                {".",        TT.DOT      },
                                {",",        TT.COMMA    },
                                {";",        TT.SEMICOLON},
                                {":",        TT.COLON    },
                                {"->",       TT.ARROW    },
                                {"(",        TT.LPAREN   },
                                {")",        TT.RPAREN   },
                                {"{",        TT.LBRACE   },
                                {"}",        TT.RBRACE   },
                                {"[",        TT.LBRACKET },
                                {"]",        TT.RBRACKET },
                                {"...",      TT.REST     },
                                {"!",        TT.BANG     },
                                {"",         TT.EOF      },
                                ];

        int nfailure = 0;

        void run_test(TestData data){
            auto lexer = new Lexer(new StringReadStream(data.src), "(test)");
            auto t = lexer.read();
            try {
                assert(t.tt == data.expected);
                assert(t.value == (data.expect_val ? data.expect_val : data.src));
            } catch (core.exception.AssertError e) {
                nfailure++;
                "%s: expect (%s), actual `%s'(%s)".format(data.src, data.expected, t.value, t.tt).writeln();
            }
        }

        "= Test Lexer".writeln();
        foreach (data; test_data) run_test(data);
        "%d tests, %d faliures.\n".format(test_data.length, nfailure).writeln();
    }
}



class Parser {
    import std.container;
    import std.range;
    import ando.node;

    private Lexer lexer;
    private SList!Token buf;
    private Node tree_;

    this (Lexer lexer) {
        this.lexer = lexer;
    }

    @property Node tree(){ return tree_; }

    void parse(){
        Token bof;
        bof.line = 0;
        tree_ = parse_block(bof, TT.EOF).fix();
    }

    private Token read(){
        if (buf.empty()) return lexer.read();
        auto ret = buf.front();
        buf.removeFront();
        return ret;
    }

    private void push_back(Token t){
        buf.insertFront(t);
    }

    private Token expect(TT expected, string dfile = __FILE__, int dline = __LINE__){
        auto t = read();
        if (t.tt != expected) {
            throw ParseError.create("expect %s but %s:`%s'".format(expected, t.tt, t.value), lexer.file, t.line, dfile, dline);
        }
        return t;
    }

    private Node parse_block(Token beg, TT term){
        Token t;
        auto b = new Block(beg.line);

        for (;;) {
            t = read();
            if (t.tt == TT.EOF) break;
            push_back(t);
            if (t.tt == term) break;
            parse_statement(b);
        }
        expect(term);

        return b;
    }

    private void parse_statement(Block current){
        auto t = read();
        switch (t.tt) {
        case TT.SEMICOLON:
            break;
        case TT.IF:
            parse_if(current, t);
            break;
        case TT.UNLESS:
            parse_unless(current, t);
            break;
        case TT.WHILE:
            parse_while(current, t);
            break;
        case TT.UNTIL:
            parse_until(current, t);
            break;
        case TT.BREAK:
            current.append(new Break(t.line));
            expect(TT.SEMICOLON);
            break;
        case TT.CONTINUE:
            current.append(new Continue(t.line));
            expect(TT.SEMICOLON);
            break;
        case TT.RETURN:
            parse_return(current, t);
            break;
        case TT.DEF:
            if (is_def_expr_stmt()) goto default;
            parse_def(current, t);
            break;
        default:
            push_back(t);
            current.append(parse_expr());
            expect(TT.SEMICOLON);
            break;
        }
    }

    private bool is_def_expr_stmt(){
        auto t = read();
        push_back(t);
        return (t.tt == TT.IDENT ? false : true);
    }

    private void parse_if(Block current, Token kw){
        current.append(parse_if_main(kw));
    }

    private Node parse_if_main(Token kw){
        auto test = parse_expr();
        auto t = expect(TT.LBRACE);
        auto then = parse_block(t, TT.RBRACE);
        Node alt;

        t = read();
        switch (t.tt) {
        case TT.ELSIF:
            alt = parse_if_main(t);
            break;
        case TT.ELSE:
            t = expect(TT.LBRACE);
            alt = parse_block(t, TT.RBRACE);
            break;
        default:
            push_back(t);
            alt = new NilLiteral(kw.line);
        }

        return new If(kw.line, test, then, alt);        
    }

    private void parse_unless(Block current, Token kw){
        auto test = parse_expr();
        auto t = expect(TT.LBRACE);
        auto then = parse_block(t, TT.RBRACE);
        Node alt;

        t = read();
        if (t.tt == TT.ELSE) {
            t = expect(TT.LBRACE);
            alt = parse_block(t, TT.RBRACE);
        } else {
            push_back(t);
            alt = new NilLiteral(kw.line);
        }

        current.append(new If(kw.line, test, alt, then));
    }

    private void parse_while(Block current, Token kw){
        auto cond = parse_expr();
        auto t = expect(TT.LBRACE);
        current.append(new While(kw.line, cond, parse_block(t, TT.RBRACE)));
    }

    private void parse_until(Block current, Token kw){
        auto cond = parse_expr();
        auto t = expect(TT.LBRACE);
        current.append(new Until(kw.line, cond, parse_block(t, TT.RBRACE)));
    }

    private void parse_return(Block current, Token kw){
        Node expr = null;
        auto t = read();
        if (t.tt != TT.SEMICOLON) {
            push_back(t);
            expr = parse_expr();
            expect(TT.SEMICOLON);
        }
        current.append(new Return(kw.line, expr));
    }

    private void parse_def(Block current, Token kw){
        void unexpected_delimiter(Token t){
            push_back(t);
            expect(TT.SEMICOLON);
        }

        bool parsing = true;

        while (parsing) {
            Node expr;
            auto var = expect(TT.IDENT);
            auto t = read();
            parsing = true;

            switch (t.tt) {
            case TT.ASSIGN:
                expr = parse_expr();
                t = read();
                if (t.tt == TT.SEMICOLON) goto case TT.SEMICOLON;
                if (t.tt == TT.COMMA) goto case TT.COMMA;
                goto default;
            case TT.SEMICOLON:
                parsing = false;
                break;
            case TT.COMMA:
                break;

            case TT.LPAREN: goto case;
            case TT.LBRACE:
                push_back(t);
                expr = parse_function_literal(kw);
                parsing = false;
                break;
            default:
                unexpected_delimiter(t);
            }

            current.append(new Def(kw.line, var.value, expr));
        }
    }

    private Node parse_expr(){
        return parse_assign();
    }

    private Node parse_assign(){
        auto left = parse_equality();
        auto t = read();
        Operators op;
        bool no_op = false;
        Node right, ret;

        switch (t.tt) {
        case TT.ASSIGN:
            no_op = true;
            break;
        case TT.ADD_A:
            op = Operators.ADD;
            break;
        case TT.SUB_A:
            op = Operators.SUB;
            break;
        case TT.MUL_A:
            op = Operators.MUL;
            break;
        case TT.DIV_A:
            op = Operators.DIV;
            break;
        case TT.MOD_A:
            op = Operators.MOD;
            break;
        default:
            push_back(t);
            return left;
        }

        if (left.is_parened) goto LVAL_NG;

        right = parse_assign();
        if (!no_op) right = new BinaryExpr(t.line, op, left, right);

        ret = left.to_assign(t.line, right);
        if (!ret) goto LVAL_NG;
        return ret;

    LVAL_NG:
        throw ParseError.create("invalid lvalue", lexer.file, t.line);
    }

    private Node parse_equality(){
        auto left = parse_compare();
        for (;;) {
            auto t = read();
            Operators op;
            switch (t.tt) {
            case TT.EQ:
                op = Operators.EQ;
                break;
            case TT.NE:
                op = Operators.NE;
                break;
            default:
                push_back(t);
                return left;
            }
            left = new BinaryExpr(t.line, op, left, parse_compare());
        }
        return left;
    }

    private Node parse_compare(){
        auto left = parse_addtive();
        for (;;) {
            auto t = read();
            Operators op;
            switch (t.tt) {
            case TT.GE:
                op = Operators.GE;
                break;
            case TT.LE:
                op = Operators.LE;
                break;
            case TT.GT:
                op = Operators.GT;
                break;
            case TT.LT:
                op = Operators.LT;
                break;
            default:
                push_back(t);
                return left;
            }
            left = new BinaryExpr(t.line, op, left, parse_addtive());
        }
        return left;
    }

    private Node parse_addtive(){
        auto left = parse_multive();
        for (;;) {
            auto t = read();
            Operators op;
            switch (t.tt) {
            case TT.ADD:
                op = Operators.ADD;
                break;
            case TT.SUB:
                op = Operators.SUB;
                break;
            default:
                push_back(t);
                return left;
            }
            left = new BinaryExpr(t.line, op, left, parse_multive());
        }
        return left;
    }

    private Node parse_multive(){
        auto left = parse_unary();
        for (;;) {
            auto t = read();
            Operators op;
            switch (t.tt) {
            case TT.MUL:
                op = Operators.MUL;
                break;
            case TT.DIV:
                op = Operators.DIV;
                break;
            case TT.MOD:
                op = Operators.MOD;
                break;
            default:
                push_back(t);
                return left;
            }
            left = new BinaryExpr(t.line, op, left, parse_unary());
        }
        return left;
    }

    private Node parse_unary(){
        Operators op;
        auto t = read();

        switch (t.tt) {
        case TT.ADD:
            op = Operators.PLUS;
            break;
        case TT.SUB:
            op = Operators.MINUS;
            break;
        case TT.BANG:
            op = Operators.NOT;
            break;
        default:
            push_back(t);
            return parse_postfix();
        }

        return new UnaryExpr(t.line, op, parse_unary());
    }

    private Node parse_postfix(){
        auto expr = parse_primary();

        for (;;) {
            auto t = read();
            switch (t.tt) {
            case TT.LPAREN:
                expr = new Apply(t.line, expr, parse_args(t, TT.RPAREN));
                break;
            case TT.LBRACKET:
                expr = new ARef(t.line, expr, parse_args(t, TT.RBRACKET));
                break;
            case TT.DOT:
                expr = new PRef(t.line, expr, expect(TT.IDENT).value);
                break;
            default:
                push_back(t);
                return expr;
            }
        }

        return expr;
    }

    private ArgList parse_args(Token beg, TT term){
        Token t = read(), nt;
        auto list = new ArgList();
        if (t.tt == term) return list;
        push_back(t);

        for (;;) {
            ArgSpec arg;
            arg.type = ArgType.ORDINARY;

            // parse keyword part
            t = read();
            if (t.tt == TT.IDENT) {
                nt = read();
                if (nt.tt == TT.COLON) {
                    arg.type = ArgType.KEYWORD;
                    arg.key = t.value;
                } else {
                    push_back(nt);
                    push_back(t);
                }
            } else {
                push_back(t);
            }

            arg.expr = parse_expr();

            t = read();
            if (t.tt == TT.REST && arg.type != ArgType.KEYWORD) {
                arg.type = ArgType.SPLAT;
                t = read();
            }

            list.append(arg);

            if (t.tt == term) {
                break;
            } else if (t.tt == TT.COMMA) {
                ;
            } else {
                push_back(t);
                expect(term);
            }
        }

        return list;
    }

    private Node parse_primary(){
        import std.conv;

        auto t = read();
        switch (t.tt) {
        case TT.LPAREN:
            auto ret = parse_expr();
            expect(TT.RPAREN);
            ret.parened();
            return ret;
        case TT.INTEGER:
            return new IntegerLiteral(t.line, t.value.to!int);
        case TT.FLOATING:
            return new FloatingLiteral(t.line, t.value.to!double);
        case TT.STRING:
            return new StringLiteral(t.line, t.value);
        case TT.TRUE:
            return new TrueLiteral(t.line);
        case TT.FALSE:
            return new FalseLiteral(t.line);
        case TT.NIL:
            return new NilLiteral(t.line);
        case TT.IDENT:
            return new VRef(t.line, t.value);
        case TT.LBRACKET:
            return parse_array_literal(t);
        case TT.LBRACE:
            return parse_table_literal(t);
        case TT.DEF:
            return parse_function_literal(t);
        default:
            break;
        }
        throw ParseError.create("unexpected %s:`%s'".format(t.tt, t.value), lexer.file, t.line);
        return null;
    }

    private Node parse_array_literal(Token lbracket){
        return new ArrayLiteral(lbracket.line, parse_args(lbracket, TT.RBRACKET));
    }

    private Node parse_table_literal(Token lbrace){
        auto pairs = new TablePairList();
        Token t = read(), nt;
        if (t.tt == TT.RBRACE) goto PAIRS_END;
        push_back(t);

        for (;;) {
            TablePair pair;

            t = read();
            if (t.tt == TT.IDENT) {
                nt = read();
                if (nt.tt == TT.COLON) {
                    pair.key = new StringLiteral(t.line, t.value);
                } else {
                    push_back(nt);
                    push_back(t);
                }
            } else {
                push_back(t);
            }

            if (!pair.key) {
                pair.key = parse_expr();
                expect(TT.ARROW);
            }
            pair.value = parse_expr();

            pairs.append(pair);

            t = read();
            if (t.tt == TT.RBRACE) {
                break;
            } else if (t.tt == TT.COMMA) {
                ;
            } else {
                push_back(t);
                expect(TT.COMMA);
            }
        }

    PAIRS_END:
        return new TableLiteral(lbrace.line, pairs);
    }

    private ParamList parse_params(Token beg, TT term = TT.RPAREN){
        Token t = read(), nt;
        auto list = new ParamList();
        if (t.tt == term) return list;
        push_back(t);

        for (;;) {
            ParamSpec param;
            param.type = ParamType.REQUIRED;
            param.name = expect(TT.IDENT).value;

            t = read();
            switch (t.tt) {
            case TT.ASSIGN:
                param.type = ParamType.OPTIONAL;
                break;
            case TT.SEMICOLON:
                param.type = ParamType.KEYWORD;
                break;
            case TT.REST:
                param.type = ParamType.REST;
                break;
            default:
                push_back(t);
            }

            switch (param.type) {
            case ParamType.OPTIONAL: goto case;
            case ParamType.KEYWORD:
                param.expr = parse_expr();
                break;
            default:
                break;
            }

            list.append(param);
 
            t = read();
            if (t.tt == term) {
                break;
            } else if (t.tt == TT.COMMA) {
                ;
            } else {
                push_back(t);
                expect(term);
            }
        }

        return list;
    }

    private Node parse_function_literal(Token kw){
        ParamList params;
        auto t = read();

        if (t.tt == TT.LPAREN) {
            params = parse_params(t);
            t = expect(TT.LBRACE);
        } else if (t.tt == TT.LBRACE) {
            params = new ParamList();
        } else {
            push_back(t);
            expect(TT.LPAREN);
        }

        return new FunctionLiteral(kw.line, params, parse_block(t, TT.RBRACE));
    }
}

