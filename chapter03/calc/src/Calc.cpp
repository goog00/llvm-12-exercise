#include "CodeGen.h"
#include "Parser.h"
#include "Sema.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

// LLVM 自带了声明命令行选项的系统，只需要为所需的每个选项声明一个静态变量。
// 在此过 程中，该选项将被注册到全局命令行解析器中。
// 这种方法的优点是，每个组件都可以在需要 时添加命令行选项。
// 我们必须为输入表达式声明一个选项
static llvm::cl::opt<std::string> Input(llvm::cl::Positional,
                                        llvm::cl::desc("<input expression>"),
                                        llvm::cl::init(""));

int main(int argc, const char **argv) {

  // LLVM 库初始化
  llvm::InitLLVM X(argc, argv);
  llvm::cl::ParseCommandLineOptions(argc, argv,
                                    "calc -the expression compiler\n");

  Lexer Lex(Input);
  Parser Parser(Lex);
  AST *Tree = Parser.parse();
  if (!Tree || Parser.hasError()) {
    llvm::errs() << "Syntax errors occured\n";
    return 1;
  }

  Sema Semantic;
  if (Semantic.semantic(Tree)) {
    llvm::errs() << "Semantic errors occured\n";
    return 1;
  }

  CodeGen CodeGenerator;

  CodeGenerator.compile(Tree);

  return 0;
}