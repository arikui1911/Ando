module ando.opcode;

enum : int {
    OP_STOP,
    OP_PUSH_TRUE,
    OP_PUSH_FALSE,
    OP_PUSH_NIL,
    OP_PUSH_INTEGER,
    OP_PUSH_FLOATING,
    OP_PUSH_STRING,
    OP_GOTO,
    OP_GOTO_IF,
    OP_GOTO_UNLESS,
}

