//语义分析器遍历 AST 并检查语言的各种语义规则
//
// 语义分析器遍历 AST 并检查语言的各种语义规则，
//例如:在使用变量之前必须声明变量，或者变量的类型必须在表达式中兼容。
//如果发现可以改进的情况，还可以输出警告。对于表达式语言,语义分析器必须检查每个使用的变量是否声明，这是语言所需。
//一个可能的扩展 (这里不实现) 是 在未使用声明的变量时输出警告消息。
#ifndef SEMA_H
#define SEMA_H

#include "AST.h"
#include "Lexer.h"

class Sema {

public:
    bool semantic(AST *Tree);
};

#endif