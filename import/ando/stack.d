module ando.stack;

class Stack(T){
    import std.container;

    private SList!T buf;

    Stack!T push(T v){
        buf.insertFront(v);
        return this;
    }

    T pop(){
        auto ret = buf.front;
        buf.removeFront();
        return ret;
    }

    T peek(){
        return buf.front;
    }

    bool empty(){
        return buf.empty();
    }
}
