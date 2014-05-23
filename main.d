import std.stdio;

import ando.readstream;
import ando.parser;
import ando.compiler;
import ando.node;


void main(){
    ando.compiler.testing();

    /*
    auto src = "
-1;
";
    auto parser = new Parser(new Lexer(new StringReadStream(src), "(test)"));
    parser.parse();
    dump_tree(parser.tree);
    */
}

