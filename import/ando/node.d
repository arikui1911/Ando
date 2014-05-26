module ando.node;

import std.container;
import std.range;
import std.string;
import std.stdio;
import ando.compiler;
import ando.opcode;
import ando.vm;

void dump_tree(Node tree){
    tree.dump(0);
}

private enum NodeTags {
    BLOCK,
    IF,
    WHILE,
    UNTIL,
    BREAK,
    CONTINUE,
    RETURN,
    DEF,
    APPLY,
    AREF,
    ASET,
    PREF,
    PSET,
    VREF,
    VSET,
    ASSIGN,
    BINARY_EXPR,
    UNARY_EXPR,
    INTEGER_LITERAL,
    FLOATING_LITERAL,
    STRING_LITERAL,
    TRUE_LITERAL,
    FALSE_LITERAL,
    NIL_LITERAL,
    ARRAY_LITERAL,
    TABLE_LITERAL,
    FUNCTION_LITERAL,
}

class Node {
    private uint line_;
    private bool parened_;

    this(uint line) {
        line_ = line;
        parened_ = false;
    }

    @property final uint line(){ return line_; }

    abstract void dump(uint nest);

    Node fix(){ return this; }

    void compile(Compiler c){}

    private final string indent(uint n){
        return "".center(n * 2);
    }

    Node to_assign(uint line, Node rvalue){
        return null;
    }

    final void parened(){ parened_ = true; }

    @property final bool is_parened(){ return parened_; }

    @property abstract NodeTags tag();
}

class Block : Node {
    import std.array;

    private Appender!(Node[]) stmts;

    this(uint line){
        super(line);
    }

    Block append(Node stmt){
        stmts.put(stmt);
        return this;
    }

    override void dump(uint nest){
        "%sBlock:%d:".format(indent(nest), line).writeln();
        foreach (s; stmts.data) {
            s.dump(nest + 1);
        }
    }

    override Node fix(){
        if (stmts.data.empty()) return new NilLiteral(line);
        if (stmts.data.length == 1) return stmts.data[0].fix();

        auto ret = new Block(line);
        foreach (s; stmts.data) {
            ret.append(s.fix());
        }
        return ret;
    }

    override void compile(Compiler c){
        foreach (s; stmts.data) {
            s.compile(c);
        }
    }

    @property override NodeTags tag(){ return NodeTags.BLOCK; }
}

class If : Node {
    private Node test;
    private Node then;
    private Node alt;

    @property override NodeTags tag(){ return NodeTags.IF; }

    this(uint line, Node test, Node then, Node alt){
        super(line);
        this.test = test;
        this.then = then;
        this.alt = alt;
    }

    override void dump(uint nest){
        "%sIf:%d:".format(indent(nest), line).writeln();
        "%s@test".format(indent(nest)).writeln();
        test.dump(nest + 1);
        "%s@then".format(indent(nest)).writeln();
        then.dump(nest + 1);
        "%s@alt".format(indent(nest)).writeln();
        alt.dump(nest + 1);
    }

    override void compile(Compiler c){
        auto alt_beg = c.get_label();
        auto end = c.get_label();
        test.compile(c);
        c.append(OP_GOTO_UNLESS).append(alt_beg.id);
        then.compile(c);
        c.append(OP_GOTO).append(end.id);
        c.assign(alt_beg);
        alt.compile(c);
        c.assign(end);
    }
}

class While : Node {
    private Node cond;
    private Node stmts;

    @property override NodeTags tag(){ return NodeTags.WHILE; }

    this(uint line, Node cond, Node stmts){
        super(line);
        this.cond = cond;
        this.stmts = stmts;
    }

    override void dump(uint nest){
        "%sWhile:%d:".format(indent(nest), line).writeln();
        "%s@cond".format(indent(nest)).writeln();
        cond.dump(nest + 1);
        "%s@stmts".format(indent(nest)).writeln();
        stmts.dump(nest + 1);
    }

    override void compile(Compiler c){
        auto cxt = c.push_loop();
        c.assign(cxt.begin);
        cond.compile(c);
        c.append(OP_GOTO_UNLESS).append(cxt.end.id);
        stmts.compile(c);
        c.append(OP_GOTO).append(cxt.begin.id);
        c.assign(cxt.end);
        c.pop_loop();
    }
}

class Until : Node {
    private Node cond;
    private Node stmts;

    @property override NodeTags tag(){ return NodeTags.UNTIL; }

    this(uint line, Node cond, Node stmts){
        super(line);
        this.cond = cond;
        this.stmts = stmts;
    }

    override void dump(uint nest){
        "%sUntil:%d:".format(indent(nest), line).writeln();
        "%s@cond".format(indent(nest)).writeln();
        cond.dump(nest + 1);
        "%s@stmts".format(indent(nest)).writeln();
        stmts.dump(nest + 1);
    }

    override void compile(Compiler c){
        auto cxt = c.push_loop();
        c.assign(cxt.begin);
        cond.compile(c);
        c.append(OP_GOTO_IF).append(cxt.end.id);
        stmts.compile(c);
        c.append(OP_GOTO).append(cxt.begin.id);
        c.assign(cxt.end);
        c.pop_loop();
    }
}

class Break : Node {
    @property override NodeTags tag(){ return NodeTags.BREAK; }

    this(uint line){
        super(line);
    }

    override void dump(uint nest){
        "%sBreak:%d:".format(indent(nest), line).writeln();
    }

    override void compile(Compiler c){
        auto cxt = c.current_loop();
        if (cxt) {
            c.append(OP_GOTO).append(cxt.end.id);
        } else {
            c.compile_error(line, "invalid break statement");
        }
    }
}

class Continue : Node {
    @property override NodeTags tag(){ return NodeTags.CONTINUE; }

    this(uint line){
        super(line);
    }

    override void dump(uint nest){
        "%sContinue:%d:".format(indent(nest), line).writeln();
    }

    override void compile(Compiler c){
        auto cxt = c.current_loop();
        if (cxt) {
            c.append(OP_GOTO).append(cxt.begin.id);
        } else {
            c.compile_error(line, "invalid continue statement");
        }
    }
}

class Return : Node {
    @property override NodeTags tag(){ return NodeTags.RETURN; }

    private Node retval;

    this(uint line, Node retval){
        super(line);
        this.retval = retval;
    }

    override void dump(uint nest){
        "%sReturn:%d:".format(indent(nest), line).writeln();
        "%s@retval".format(indent(nest)).writeln();
        if (retval) {
            retval.dump(nest + 1);
        } else {
            "%snull".format(indent(nest + 1)).writeln();
        }
    }
}

class WrappedList(T) {
    import std.array;

    private Appender!(T[]) list;

    WrappedList!(T) append(T e){
        list.put(e);
        return this;
    }

    int opApply(int delegate(ref T) dg){
        int r = 0;
        foreach (e; list.data) {
            r = dg(e);
            if (r) break;
        }
        return r;
    }

    int opApply(int delegate(int, ref T) dg){
        int r = 0;
        foreach (i, e; list.data) {
            r = dg(i, e);
            if (r) break;
        }
        return r;
    }
}

enum ArgType {
    ORDINARY,
    KEYWORD,
    SPLAT,
}

struct ArgSpec {
    ArgType type;
    Node expr;
    string key;
}

alias WrappedList!ArgSpec ArgList;

enum ParamType {
    REQUIRED,
    OPTIONAL,
    KEYWORD,
    REST,
}

struct ParamSpec {
    ParamType type;
    string name;
    Node expr;
}

alias WrappedList!ParamSpec ParamList;

struct TablePair {
    Node key;
    Node value;
}

alias WrappedList!TablePair TablePairList;

class Def : Node {
    @property override NodeTags tag(){ return NodeTags.DEF; }

    private string var;
    private Node initval;

    this(uint line, string var, Node initval){
        super(line);
        this.var = var;
        this.initval = initval;
    }

    override void dump(uint nest){
        "%sDef:%d:".format(indent(nest), line).writeln();
        "%s@var %s".format(indent(nest), var).writeln();
        "%s@initval".format(indent(nest)).writeln();
        if (initval) {
            initval.dump(nest + 1);
        } else {
            "%snull".format(indent(nest + 1)).writeln();
        }
    }
}

class Apply : Node {
    @property override NodeTags tag(){ return NodeTags.APPLY; }

    private Node func;
    private ArgList args;

    this(uint line, Node func, ArgList args){
        super(line);
        this.func = func;
        this.args = args;
    }

    override void dump(uint nest){
        "%sApply:%d:".format(indent(nest), line).writeln();
        "%s@func".format(indent(nest)).writeln();
        func.dump(nest + 1);
        foreach (i, arg; args) {
            if (arg.type == ArgType.KEYWORD) {
                "%s@arg%d: %s:%s".format(indent(nest), i, arg.type, arg.key).writeln();
            } else {
                "%s@arg%d: %s".format(indent(nest), i, arg.type).writeln();
            }
            arg.expr.dump(nest + 1);
        }
    }
}

class ARef : Node {
    @property override NodeTags tag(){ return NodeTags.AREF; }

    private Node array;
    private ArgList args;

    this(uint line, Node array, ArgList args){
        super(line);
        this.array = array;
        this.args = args;
    }

    override void dump(uint nest){
        "%sARef:%d:".format(indent(nest), line).writeln();
        "%s@array".format(indent(nest)).writeln();
        array.dump(nest + 1);
        foreach (i, arg; args) {
            if (arg.type == ArgType.KEYWORD) {
                "%s@arg%d: %s:%s".format(indent(nest), i, arg.type, arg.key).writeln();
            } else {
                "%s@arg%d: %s".format(indent(nest), i, arg.type).writeln();
            }
            arg.expr.dump(nest + 1);
        }
    }

    override Node to_assign(uint line, Node right){
        return new ASet(line, array, args, right);
    }
}

class ASet : Node {
    private Node array;
    private ArgList args;
    private Node right;

    @property override NodeTags tag(){ return NodeTags.ASET; }

    this(uint line, Node array, ArgList args, Node right){
        super(line);
        this.array = array;
        this.args = args;
        this.right = right;
    }

    override void dump(uint nest){
        "%sASet:%d:".format(indent(nest), line).writeln();
        "%s@array".format(indent(nest)).writeln();
        array.dump(nest + 1);
        foreach (i, arg; args) {
            if (arg.type == ArgType.KEYWORD) {
                "%s@arg%d: %s:%s".format(indent(nest), i, arg.type, arg.key).writeln();
            } else {
                "%s@arg%d: %s".format(indent(nest), i, arg.type).writeln();
            }
            arg.expr.dump(nest + 1);
        }
        "%s@right".format(indent(nest)).writeln();
        right.dump(nest + 1);
    }
}

class PRef : Node {
    private Node table;
    private string key;

    @property override NodeTags tag(){ return NodeTags.PREF; }

    this(uint line, Node table, string key){
        super(line);
        this.table = table;
        this.key = key;
    }

    override void dump(uint nest){
        "%sPRef:%d:".format(indent(nest), line).writeln();
        "%s@table".format(indent(nest)).writeln();
        table.dump(nest + 1);
        "%s@key %s".format(indent(nest), key).writeln();
    }

    override Node to_assign(uint line, Node right){
        return new PSet(line, table, key, right);
    }
}

class PSet : Node {
    private Node table;
    private string key;
    private Node right;

    @property override NodeTags tag(){ return NodeTags.PSET; }

    this(uint line, Node table, string key, Node right){
        super(line);
        this.table = table;
        this.key = key;
        this.right = right;
    }

    override void dump(uint nest){
        "%sPSet:%d:".format(indent(nest), line).writeln();
        "%s@table".format(indent(nest)).writeln();
        table.dump(nest + 1);
        "%s@key %s".format(indent(nest), key).writeln();
        "%s@right".format(indent(nest)).writeln();
        right.dump(nest + 1);
    }
}

class VRef : Node {
    private string var;

    @property override NodeTags tag(){ return NodeTags.VREF; }

    this(uint line, string var){
        super(line);
        this.var = var;
    }

    override void dump(uint nest){
        "%sVRef:%d:".format(indent(nest), line).writeln();
        "%s@var %s".format(indent(nest), var).writeln();
    }

    override Node to_assign(uint line, Node right){
        return new VSet(line, var, right);
    }
}

class VSet : Node {
    private string var;
    private Node right;

    @property override NodeTags tag(){ return NodeTags.VSET; }

    this (uint line, string var, Node right) {
        super(line);
        this.var = var;
        this.right = right;
    }

    override void dump(uint nest){
        "%sVSet:%d:".format(indent(nest), line).writeln();
        "%s@var %s".format(indent(nest), var).writeln();
        "%s@right".format(indent(nest)).writeln();
        right.dump(nest + 1);
    }
}

class Assign : Node {
    private Node left;
    private Node right;

    @property override NodeTags tag(){ return NodeTags.ASSIGN; }

    this(uint line, Node left, Node right){
        super(line);
        this.left = left;
        this.right = right;
    }

    override void dump(uint nest){
        "%sAssign:%d:".format(indent(nest), line).writeln();
        "%s@left".format(indent(nest)).writeln();
        left.dump(nest + 1);
        "%s@right".format(indent(nest)).writeln();
        right.dump(nest + 1);
    }
}

enum Operators {
    EQ,
    NE,
    GE,
    LE,
    GT,
    LT,
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    PLUS,
    MINUS,
    NOT,
}

private bool do_compare(T)(Operators op, T x, T y){
    switch (op){
    case Operators.EQ: return x == y;
    case Operators.NE: return x != y;
    case Operators.GE: return x >= y;
    case Operators.LE: return x <= y;
    case Operators.GT: return x > y;
    case Operators.LT: return x < y;
    default:
        assert(0);
    }
}

private T do_arith(T)(Operators op, T x, T y){
    switch (op){
    case Operators.ADD: return lval + rval;
    case Operators.SUB: return lval - rval;
    case Operators.MUL: return lval * rval;
    case Operators.DIV: return lval / rval;
    case Operators.MOD: return lval % rval;
    default:
        assert(0);
    }
}

class BinaryExpr : Node {
    import std.conv;

    private Operators op;
    private Node left;
    private Node right;

    @property override NodeTags tag(){ return NodeTags.BINARY_EXPR; }

    this(uint line, Operators op, Node left, Node right){
        super(line);
        this.op = op;
        this.left = left;
        this.right = right;
    }

    override Node fix(){
        left = left.fix();
        right = right.fix();

        if (left.tag == NodeTags.INTEGER_LITERAL) {
            int rval;

            if (right.tag == NodeTags.INTEGER_LITERAL) {
                rval = (cast(IntegerLiteral)right).value;
            } else if (right.tag == NodeTags.FLOATING_LITERAL) {
                rval = (cast(FloatingLiteral)right).value.to!int();
            } else {
                return this;
            }

            int lval = (cast(IntegerLiteral)left).value;

            if (op < Operators.ADD) {
                return do_compare!int(op, lval, rval) ? new TrueLiteral(line) : new FalseLiteral(line);
            } else {
                return new IntegerLiteral(line, do_arith!int(op, lval, rval));
            }
        } else if (left.tag == NodeTags.FLOATING_LITERAL) {
            double rval;

            if (right.tag == NodeTags.INTEGER_LITERAL) {
                rval = (cast(IntegerLiteral)right).value.to!double();
            } else if (right.tag == NodeTags.FLOATING_LITERAL) {
                rval = (cast(FloatingLiteral)right).value;
            } else {
                return this;
            }

            double lval = (cast(FloatingLiteral)left).value;

            if (op < Operators.ADD) {
                return do_compare!double(op, lval, rval) ? new TrueLiteral(line) : new FalseLiteral(line);
            } else {
                return new FloatingLiteral(line, do_arith!double(op, lval, rval));
            }
        } else if (left.tag == NodeTags.STRING_LITERAL) {
            string str;

            if (op != Operators.ADD) return this;

            if (right.tag == NodeTags.INTEGER_LITERAL) {
                str = (cast(IntegerLiteral)right).value.to!string;
            } else if (right.tag == NodeTags.FLOATING_LITERAL) {
                str = (cast(FloatingLiteral)right).value.to!string;
            } else if (right.tag == NodeTags.STRING_LITERAL) {
                str = (cast(StringLiteral)right).value;
            } else {
                return this;
            }

            return new StringLiteral(line, (cast(StringLiteral)left).value ~ str);
        }

        return this;
    }

    override void dump(uint nest){
        "%sBinaryExpr:%d:".format(indent(nest), line).writeln();
        "%s@op %s".format(indent(nest), op).writeln();
        "%s@left".format(indent(nest)).writeln();
        left.dump(nest + 1);
        "%s@right".format(indent(nest)).writeln();
        right.dump(nest + 1);
    }
}

class UnaryExpr : Node {
    private Operators op;
    private Node expr;

    @property override NodeTags tag(){ return NodeTags.UNARY_EXPR; }

    this(uint line, Operators op, Node expr){
        super(line);
        this.op = op;
        this.expr = expr;
    }

    override Node fix(){
        expr = expr.fix();
        if (op == Operators.NOT) {
            switch (expr.tag) {
            case NodeTags.INTEGER_LITERAL:  goto case;
            case NodeTags.FLOATING_LITERAL: goto case;
            case NodeTags.STRING_LITERAL:   goto case;
            case NodeTags.TRUE_LITERAL:
                return new FalseLiteral(line);
            case NodeTags.FALSE_LITERAL: goto case;
            case NodeTags.NIL_LITERAL:
                return new TrueLiteral(line);
            default:
                break;
            }
        } else if (op == Operators.PLUS) {
            switch (expr.tag) {
            case NodeTags.INTEGER_LITERAL:
                return new IntegerLiteral(line, (cast(IntegerLiteral)expr).value);
            case NodeTags.FLOATING_LITERAL:
                return new FloatingLiteral(line, (cast(FloatingLiteral)expr).value);
            default:
                break;
            }
        } else if (op == Operators.MINUS) {
            switch (expr.tag) {
            case NodeTags.INTEGER_LITERAL:
                return new IntegerLiteral(line, -((cast(IntegerLiteral)expr).value));
            case NodeTags.FLOATING_LITERAL:
                return new FloatingLiteral(line, -((cast(FloatingLiteral)expr).value));
            default:
                break;
            }
        }
        return this;
    }

    override void dump(uint nest){
        "%sUnaryExpr:%d:".format(indent(nest), line).writeln();
        "%s@op %s".format(indent(nest), op).writeln();
        "%s@expr".format(indent(nest)).writeln();
        expr.dump(nest + 1);
    }
}

class IntegerLiteral : Node {
    private int val;

    @property override NodeTags tag(){ return NodeTags.INTEGER_LITERAL; }

    this(uint line, int val){
        super(line);
        this.val = val;
    }

    @property int value(){ return val; }

    override void dump(uint nest){
        "%sIntegerLiteral:%d:".format(indent(nest), line).writeln();
        "%s@val %d".format(indent(nest), val).writeln();
    }

    override void compile(Compiler c){
        c.append(OP_PUSH_INTEGER).append(val);
    }
}

class FloatingLiteral : Node {
    @property override NodeTags tag(){ return NodeTags.FLOATING_LITERAL; }

    private double val;

    this(uint line, double val){
        super(line);
        this.val = val;
    }

    @property double value(){ return val; }

    override void dump(uint nest){
        "%sFloatingLiteral:%d:".format(indent(nest), line).writeln();
        "%s@val %f".format(indent(nest), val).writeln();
    }
}

class StringLiteral : Node {
    private string val;

    @property override NodeTags tag(){ return NodeTags.STRING_LITERAL; }

    this(uint line, string val){
        super(line);
        this.val = val;
    }

    @property string value(){ return val; }

    override void dump(uint nest){
        "%sStringLiteral:%d:".format(indent(nest), line).writeln();
        "%s@val \"%s\"".format(indent(nest), val).writeln();
    }
}

class TrueLiteral : Node {
    @property override NodeTags tag(){ return NodeTags.TRUE_LITERAL; }

    this(uint line){
        super(line);
    }

    override void dump(uint nest){
        "%sTrueLiteral:%d:".format(indent(nest), line).writeln();
    }

    override void compile(Compiler c){
        c.append(OP_PUSH_TRUE);
    }
}

class FalseLiteral : Node {
    @property override NodeTags tag(){ return NodeTags.FALSE_LITERAL; }

    this(uint line){
        super(line);
    }

    override void dump(uint nest){
        "%sFalseLiteral:%d:".format(indent(nest), line).writeln();
    }

    override void compile(Compiler c){
        c.append(OP_PUSH_FALSE);
    }
}

class NilLiteral : Node {
    @property override NodeTags tag(){ return NodeTags.NIL_LITERAL; }

    this(uint line){
        super(line);
    }

    override void dump(uint nest){
        "%sNilLiteral:%d:".format(indent(nest), line).writeln();
    }

    override void compile(Compiler c){
        c.append(OP_PUSH_NIL);
    }
}

class ArrayLiteral : Node {
    private ArgList args;

    @property override NodeTags tag(){ return NodeTags.ARRAY_LITERAL; }

    this(uint line, ArgList args){
        super(line);
        this.args = args;
    }

    override void dump(uint nest){
        "%sArrayLiteral:%d:".format(indent(nest), line).writeln();
        foreach (i, arg; args) {
            if (arg.type == ArgType.KEYWORD) {
                "%s@arg%d: %s:%s".format(indent(nest), i, arg.type, arg.key).writeln();
            } else {
                "%s@arg%d: %s".format(indent(nest), i, arg.type).writeln();
            }
            arg.expr.dump(nest + 1);
        }
    }
    
    override void compile(Compiler c){
    }
}

class TableLiteral : Node {
    private TablePairList pairs;

    @property override NodeTags tag(){ return NodeTags.TABLE_LITERAL; }

    this(uint line, TablePairList pairs){
        super(line);
        this.pairs = pairs;
    }

    override void dump(uint nest){
        "%sTableLiteral:%d:".format(indent(nest), line).writeln();
        foreach (i, pair; pairs) {
            "%s@key%d".format(indent(nest), i).writeln();
            pair.key.dump(nest + 1);
            "%s@value%d".format(indent(nest), i).writeln();
            pair.value.dump(nest + 1);
        }
    }
}

class FunctionLiteral : Node {
    private ParamList params;
    private Node stmts;

    @property override NodeTags tag(){ return NodeTags.FUNCTION_LITERAL; }

    this(uint line, ParamList params, Node stmts){
        super(line);
        this.params = params;
        this.stmts = stmts;
    }

    override void dump(uint nest){
        "%sFunctionLiteral:%d:".format(indent(nest), line).writeln();
        foreach (i, param; params) {
            "%s@param%d: %s %s".format(indent(nest), i, param.type, param.name).writeln();
            if (param.expr) param.expr.dump(nest + 1);
        }
        "%s@stmts".format(indent(nest)).writeln();
        stmts.dump(nest + 1);
    }
}

