#ifndef LEXER_H
#define LEXER_H


#include "llvm/ADT/StringRef.h"
#include "llvm/Support/MemoryBuffer.h" //提供对一个内存块的只读访问

class Lexer;

class Token {
    friend class Lexer;

public:
    /// 令牌类包含令牌编号
    enum TokenKind : unsigned short {
        eoi,//eoi 表示输入的结束
        unknown,//unknown 用于词汇错误的情况
        ident,
        number,
        comma,
        colon,
        plus,
        minus,
        star,
        l_paren,
        r_paren,
        KW_with
    };


private:
    TokenKind Kind;
    llvm::StringRef Text;

public:
    TokenKind getKind() const{return Kind;} 
    llvm::StringRef getText() const{
        return Text;
    }   

    bool is(TokenKind K) const{ return Kind == K;}       
    bool isOneOf(TokenKind K1,TokenKind K2) const{
        return is(K1) || is(K2);
    } 

    template <typename... Ts>
    bool isOneOf(TokenKind K1,TokenKind K2,Ts... Ks) const{
        return is(K1) || isOneOf(K2,Ks...);
    }

};


class Lexer {
    const char *BufferStart;
    const char *BufferPtr;

public:
    Lexer(const llvm::StringRef &Buffer){
        BufferStart = Buffer.begin();
        BufferPtr = BufferStart;
    }
    void next(Token &token);

private:
    void formToken(Token &Result,const char *TokEnd,Token::TokenKind Kind);

};

#endif
