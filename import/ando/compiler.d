module ando.compiler;

import ando.node;

class Label {
    private int id_;
    private int addr;

    this(int id){
        id_ = id;
    }

    @property int id(){ return id_; }

    void assign(int addr){
        this.addr = addr;
    }
}

class LoopContext {
    Label beg_;
    Label end_;

    this (Label beg, Label end) {
        beg_ = beg;
        end_ = end;
    }

    @property Label begin(){ return beg_; }
    @property Label end(){ return end_; }
}

class Compiler {
    import std.array;
    import ando.stack;

    private Appender!(int[]) codes;
    private Appender!(Label[]) labels;
    private Stack!LoopContext loops;

    void compile(Node tree){
        tree.compile(this);
    }

    Compiler append(int code){
        codes.put(code);
        return this;
    }

    void compile_error(uint line, string msg){
        throw new Exception(msg);
    }

    Label get_label(){
        auto lb = new Label(labels.data.length);
        labels.put(lb);
        return lb;
    }

    void assign(Label lb){
        lb.assign(codes.data.length);
    }

    LoopContext push_loop(){
        auto cxt = new LoopContext(get_label(), get_label());
        loops.push(cxt);
        return cxt;
    }

    LoopContext current_loop(){
        return loops.empty() ? null : loops.peek();
    }

    void pop_loop(){
        loops.pop();
    }
}



void testing(){
    import ando.readstream;
    import ando.parser;

    auto src = "

1;

";

    auto parser = new Parser(new Lexer(new StringReadStream(src), "(test)"));
    parser.parse();

    auto c = new Compiler();
    c.compile(parser.tree);
}

/*

  コンスタントプール
  ヘッダ1
  命令列1
  ヘッダ2
  命令列2
  ...



 */
