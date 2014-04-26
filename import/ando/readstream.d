module ando.readstream;

import std.stream;


abstract class ReadStream {
    private char buf;
    private bool buf_used;
    private bool eof_p;

    this(){
        eof_p = false;
        buf_used = false;
    }

    @property final char EOF(){ return char.init; }

    char raw_getch();

    final char getch(){
        if (buf_used) {
            buf_used = false;
            return buf;
        } else if (eof_p) {
            return this.EOF;
        } else {
            return raw_getch();
        }
    }

    final void ungetch(char c){
        buf_used = true;
        buf = c;
    }

    final private void on_eof(){ eof_p = true; }

    @property final bool is_eof(){
        return !buf_used && eof_p;
    }
}

class StringReadStream : ReadStream {
    private string source;
    private uint pos;

    this(string src){
        super();
        source = src;
        pos = 0;
    }

    override char raw_getch(){
        if (pos == source.length) {
            on_eof();
            return this.EOF;
        } else {
            return source[pos++];
        }
    }

    unittest {
        auto s = new StringReadStream("b");
        s.ungetch('a');
        assert(s.getch() == 'a');
        assert(s.getch() == 'b');
        assert(s.getch() == char.init);
        assert(s.is_eof);
    }
}

class FileReadStream : ReadStream {
    private std.stream.File source;

    this (std.stream.File src) {
        source = src;
    }

    this(string filename){
        this(new std.stream.File(filename));
    }

    override char raw_getch(){
        return source.getc();
    }
}

