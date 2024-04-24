#include "CodeGen.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"

using namespace llvm;

namespace {
class ToIRVisitor : public ASTVisitor {
  Module *M;
  IRBuilder<> Builder;
  Type *VoidTy;
  Type *Int32Ty;
  Type *Int8PtrTy;
  Type *Int8PtrPtrTy;
  Constant *Int32Zero;

  Value *V;
  StringMap<Value *> nameMap;

public:
  // 构造函数
  ToIRVisitor(Module *M) : M(M), Builder(M->getContext()) {
    VoidTy = Type::getVoidTy(M->getContext());
    Int32Ty = Type::getInt32Ty(M->getContext());
    Int8PtrTy = Type::getInt8PtrTy(M->getContext());
    Int8PtrPtrTy = Int8PtrTy->getPointerTo();
    Int32Zero = ConstantInt::get(Int32Ty, 0, true);
  }

  void run(AST *Tree) {
    // 对于每个函数，都必须创建一个 FunctionType 实例
    // 定义了 LLVM IR 中的 main() 函数:
    FunctionType *MainFty =
        FunctionType::get(Int32Ty, {Int32Ty, Int8PtrPtrTy}, false);

    Function *MainFn =
        Function::Create(MainFty, GlobalValue::ExternalLinkage, "main", M);

    // 创建带有入口标签的 BB 基本块，并将其附加到 IR 构建器上
    BasicBlock *BB = BasicBlock::Create(M->getContext(), "entry", MainFn);
    Builder.SetInsertPoint(BB);

    // 开始进行树形遍历
    Tree->accept(*this);

    // 遍历树后，通过 calc write() 函数打印计算值。
    FunctionType *CalcWriterFnTy = FunctionType::get(VoidTy, {Int32Ty}, false);

    Function *CalcWriteFn = Function::Create(
        CalcWriterFnTy, GlobalValue::ExternalLinkage, "calc_write", M);
    Builder.CreateCall(CalcWriterFnTy, CalcWriteFn, {V});

    // 生成结束时，main() 函数返回 0:
    Builder.CreateRet(Int32Zero);
  }

  virtual void visit(Factor &Node) override {
    if (Node.getKind() == Factor::Ident) {
      V = nameMap[Node.getVal()];
    } else {
      int intval;
      Node.getVal().getAsInteger(10, intval);
      V = ConstantInt::get(Int32Ty, intval, true);
    }
  };

  virtual void visit(BinaryOp &Node) override {
    Node.getLeft()->accept(*this);
    Value *Left = V;
    Node.getRight()->accept(*this);
    Value *Right = V;
    switch (Node.getOperator()) {
    case BinaryOp::Plus:
      V = Builder.CreateNSWAdd(Left, Right);
      break;
    case BinaryOp::Minus:
      V = Builder.CreateNSWSub(Left, Right);
      break;
    case BinaryOp::Mul:
      V = Builder.CreateNSWMul(Left, Right);
      break;
    case BinaryOp::Div:
      V = Builder.CreateSDiv(Left, Right);
      break;
    }
  };

  // WithDecl 节点保存声明变量的名称
  virtual void visit(WithDecl &Node) override {
    // 为 calc read() 函数创建一个函数原型
    FunctionType *ReadFty = FunctionType::get(Int32Ty, {Int8PtrTy}, false);

    Function *ReadFn =
        Function::Create(ReadFty, GlobalValue::ExternalLinkage, "calc_read", M);

    // 遍历变量名 ,对于每个变量，都会创建一个带有变量名称的字符串
    for (auto I = Node.begin(), E = Node.end(); I != E; ++I) {
      StringRef Var = *I;

      //
      Constant *StrText = ConstantDataArray::getString(M->getContext(), Var);
      GlobalVariable *Str = new GlobalVariable(*M, StrText->getType(), true,
                                         GlobalValue::PrivateLinkage, StrText,
                                         Twine(Var).concat(".str"));
      Value *Ptr =
          Builder.CreateInBoundsGEP(
                Str, {Int32Zero, Int32Zero}, "ptr");
      CallInst *Call = Builder.CreateCall(ReadFty, ReadFn, {Ptr});

      nameMap[Var] = Call;
    }

    Node.getExpr()->accept(*this);
  };
};

} // namespace

void CodeGen::compile(AST *Tree) {
  LLVMContext Ctx;
  Module *M = new Module("calc.expr", Ctx);

  ToIRVisitor ToIR(M);
  ToIR.run(Tree);
  M->print(outs(), nullptr);
}