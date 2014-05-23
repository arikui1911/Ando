module ando.vm;

import ando.opcode;
import ando.stack;

class Value {
}

class Integer : Value {
    private int value;

    this(int n){
        this.value = n;
    }
}


class VM {
    private Stack!Value stack;
    private int[] codes;
    private uint pc;

    this(){
        stack = new Stack!Value();
        pc = 0;
    }

    int intern(string s){
        return 0;
    }

    void run(){
    RUN_LOOP:
        while (pc < codes.length) {
            final switch (codes[pc]) {
            case OP_STOP:
                break RUN_LOOP;
            case OP_PUSH_INTEGER:
                stack.push(new Integer(codes[pc + 1]));
                pc += 2;
                break;
            case OP_PUSH_FLOATING:
                pc += 2;
                break;
            case OP_PUSH_STRING:
                pc += 2;
                break;
            }
        }
    }
}

