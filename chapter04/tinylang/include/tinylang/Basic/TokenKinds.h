#ifndef TINYLANG_BASIC_TOKENKINDS_H
#define TINYLANG_BASIC_TOKENKINDS_H

#include "llvm/Support/Compiler.h"

namespace tinylang {
namespace tok{
enum  TokenKind : unsigned short {
    #define TOK(ID) ID,
    #include "TokenKinds.def"
    NUM_TOKENS
};

//在这些方法后面写 `LLVM_READNONE` 是一种用于指示函数属性的标记。

//`LLVM_READNONE` 表示该函数不会读取任何内存（即不进行读操作）。这是一种用于提高代码安全性和优化的指示。

///它可以帮助编译器进行更精确的分析和优化，确保函数的行为符合预期。
const char *getTokenName(TokenKind Kind) LLVM_READNONE;

const char * 
getPunctuatorSpelling(TokenKind Kind) LLVM_READNONE;

const char *
getKeywordSpelling(TokenKind Kind) LLVM_READNONE;


}   //namespace tok

} //namespace tinylang

#endif