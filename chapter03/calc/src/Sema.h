//语义分析器遍历 AST 并检查语言的各种语义规则
#ifndef SEMA_H
#define SEMA_H

#include "AST.h"
#include "Lexer.h"

class Sema {

public:
    bool semantic(AST *Tree);
};

#endif