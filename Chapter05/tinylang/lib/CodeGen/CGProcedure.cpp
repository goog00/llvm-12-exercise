#include "tinylang/CodeGen/CGProcedure.h"
#include "llvm/IR/CFG.h"
#include "llvm/Support/Casting.h"

using namespace tinylang;

void CGProcedure::writeLocalVariable(llvm::BasicBlock *BB,
                                     Decl *Decl,
                                     llvm::Value *Val) {
  assert(BB && "Basic block is nullptr");
  assert(
      (llvm::isa<VariableDeclaration>(Decl) ||
       llvm::isa<FormalParameterDeclaration>(Decl)) &&
      "Declaration must be variable or formal parameter");
  assert(Val && "Value is nullPtr");
  CurrentDef[BB].Defs[Decl] = Val;
}

llvm::Value *
CGProcedure::readLocalVariable(llvm::BasicBlock *BB,
                               Decl *Decl) {
  assert(BB && "Basic block is nullptr");

  assert(
      (llvm::isa<VariableDeclaration>(Decl) ||
       llvm::isa<FormalParameterDeclaration>(Decl)) &&
      "Declaration must be variable or formal parameter");

  auto Val = CurrentDef[BB].Defs.find(Decl);
  if (Val != CurrentDef[BB].Defs.end())
    return Val->second;
  return readLocalVariableRecursive(BB, Decl);
}

llvm::Value *CGProcedure::readLocalVariableRecursive(
    llvm::BasicBlock *BB, Decl *Decl) {

  llvm::Value *Val = nullptr;
  if (!CurrentDef[BB].Sealed) {

    llvm::PHINode *Phi = addEmptyPhi(BB, Decl);
    CurrentDef[BB].IncompletePhis[Phi] = Decl;
    Val = Phi;
  } else if (auto *PreBB = BB->getSinglePredecessor()) {
    Val = readLocalVariable(PreBB, Decl);
  } else {
    llvm::PHINode *Phi = addEmptyPhi(BB, Decl);
    Val = Phi;
    writeLocalVariable(BB, Decl, Val);
    addPhiOperands(BB, Decl, Phi);
  }
  writeLocalVariable(BB, Decl, Val);
  return Val;
}

llvm::PHINode *
CGProcedure::addEmptyPhi(llvm::BasicBlock *BB, Decl *Decl) {
  return BB->empty()
             ? llvm::PHINode::Create(mapType(Decl), 0, "",
                                     BB)
             : llvm::PHINode::Create(mapType(Decl), 0, "",
                                     &BB->front());
}

void CGProcedure::addPhiOperands(llvm::BasicBlock *BB,
                                 Decl *Decl,
                                 llvm::PHINode *Phi) {
  for (auto I = llvm::pred_begin(BB),
            E = llvm::pred_end(BB);
       I != E; ++I) {
    Phi->addIncoming(readLocalVariable(*I, Decl), *I);
  }
  optimizePhi(Phi);
}

// 如果指令只有一个操作数或所有操作数都有相同的值，则用这个值替换指令。
// 如果指令没有 操作数，则用特殊值 Undef 替换该指令。
// 只有当指令有两个或多个不同的操作数时，才需要 保留指令:
void CGProcedure::optimizePhi(llvm::PHINode *Phi) {
  llvm::Value *Same = nullptr;
  // Phi->incoming_values() 获取操作数
  for (llvm::Value *V : Phi->incoming_values()) {
    if (V == Same || V == Phi) {
      continue;
    }
    if (Same && V != Same)
      return;
    Same = V;
  }

  if (Same == nullptr) {
    Same = llvm::UndefValue::get(Phi->getType());
  }

  llvm::SmallVector<llvm::PHINode *, 8> CandidatePhis;
  for (llvm::Use &U : Phi->uses()) {
    if (auto *P =
            llvm::dyn_cast<llvm::PHINode>(U.getUser()))

      if (P != Phi)
        CandidatePhis.push_back(P);
  }
  Phi->replaceAllUsesWith(Same);
  Phi->eraseFromParent();
  for (auto *P : CandidatePhis)
    optimizePhi(P);
}

void CGProcedure::sealBlock(llvm::BasicBlock *BB) {
  assert(!CurrentDef[BB].Sealed &&
         "Attempt to seal already sealed block");

  for (auto PhiDecl : CurrentDef[BB].IncompletePhis) {
    addPhiOperands(BB, PhiDecl.second, PhiDecl.first);
  }
  CurrentDef[BB].IncompletePhis.clear();
  CurrentDef[BB].Sealed = true;
}

llvm::Value *CGProcedure::readVariable(llvm::BasicBlock *BB,
                                       Decl *D) {

  if (auto *V = llvm::dyn_cast<VariableDeclaration>(D)) {
    if (V->getEnclosingDecl() == Proc)
      return readLocalVariable(BB, D);
    else if (V->getEnclosingDecl() ==
             CGM.getModuleDeclaration()) {
      return Builder.CreateLoad(mapType(D),
                                CGM.getGlobal(D));
    } else {
      llvm::report_fatal_error(
          "Nested procedures not yet supported");
    }
  } else if (auto *FP =
                 llvm::dyn_cast<FormalParameterDeclaration>(
                     D)) {
    if (FP->isVar()) {
      return Builder.CreateLoad(
          mapType(FP)->getPointerElementType(),
          FormalParams[FP]);
    } else {
      return readLocalVariable(BB, D);
    }
  } else {
    llvm::report_fatal_error("Unsupported declaration");
  }
}

llvm::Type *CGProcedure::mapType(Decl *Decl) {
  if (auto *FP = llvm::dyn_cast<FormalParameterDeclaration>(
          Decl)) {
    llvm::Type *Ty = CGM.convertType(FP->getType());
    if (FP->isVar()) {
      Ty = Ty->getPointerTo();
    }
    return Ty;
  }

  if (auto *V = llvm::dyn_cast<VariableDeclaration>(Decl))
    return CGM.convertType(V->getType());

  return CGM.convertType(llvm::cast<TypeDeclaration>(Decl));
}

llvm::FunctionType *CGProcedure::createFunctionType(
    ProcedureDeclaration *Proc) {
  llvm::Type *ResultTy = CGM.VoidTy;
  if (Proc->getRetType()) {
    ResultTy = mapType(Proc->getRetType());
  }

  auto FormalParams = Proc->getFormalParams();
  llvm::SmallVector<llvm::Type *, 8> ParamTypes;
  for (auto FP : FormalParams) {
    llvm::Type *Ty = mapType(FP);
    ParamTypes.push_back(Ty);
  }

  return llvm::FunctionType::get(ResultTy, ParamTypes,
                                 false);
}

llvm::Function *
CGProcedure::createFunction(ProcedureDeclaration *Proc,
                            llvm::FunctionType *FTy) {
  llvm::Function *Fn = llvm::Function::Create(
      Fty, llvm::GlobalValue::ExternalLinkage,
      CGM.mangleName(Proc), CGM.getModule());

  size_t Idx = 0;
  for (auto I = Fn->arg_begin(), E = Fn->arg_end(); I != E;
       ++I, ++Idx) {
    llvm::Argument *Arg = I;
    FormalParameterDeclaration *FP =
        Proc->getFormalParams()[Idx];

    if (FP->isVar()) {
      llvm::AttrBuilder Attr;
      llvm::TypeSize Sz =
          CGM.getModule()->getDataLayout().getTypeStoreSize(
              CGM.convertType(FP->getType()));
      Attr.addDereferenceableAttr(Sz);
      Attr.addAttribute(llvm::Attribute::NoCapture);
      Arg->addAttrs(Attr);
    }
    Arg->setName(FP->getName());
  }
  return Fn;
}

llvm::Value *
CGProcedure::emitInfixExpr(InfixExpression *E) {
  llvm::Value *Left = emitExpr(E->getLeft());
  llvm::Value *Right = emitExpr(E->getRight());
  llvm::Value *Result = nullptr;

  switch (E->getOperatorInfo().getKind()) {
  case tok::plus:
    Result = Builder.CreateNSWAdd(Left, Right);
    break;
  case tok::minus:
    Result = Builder.CreateNSWSub(Left, Right);
    break;
  case tok::star:
    Result = Builder.CreateNSWMul(Left, Right);
    break;
  case tok::kw_DIV:
    Result = Builder.CreateSDiv(Left, Right);
    break;
  case tok::kw_MOD:
    Result = Builder.CreateSRem(Left, Right);
    break;
  case tok::equal:
    Result = Builder.CreateICmpEQ(Left, Right);
    break;
  case tok::hash:
    Result = Builder.CreateICmpNE(Left, Right);
    break;
  case tok::less:
    Result = Builder.CreateICmpSLT(Left, Right);
    break;
  case tok::lessequal:
    Result = Builder.CreateICmpSLE(Left, Right);
    break;
  case tok::greater:
    Result = Builder.CreateICmpSGT(Left, Right);
    break;
  case tok::greaterequal:
    Result = Builder.CreateICmpSGE(Left, Right);
    break;
  case tok::kw_AND:
    Result = Builder.CreateAnd(Left, Right);
    break;
  case tok::kw_OR:
    Result = Builder.CreateOr(Left, Right);
    break;
  case tok::slash:
    // Divide by real numbers not supported.
    LLVM_FALLTHROUGH;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}

llvm::Value *CGProcedure::emitPrefixExpr(PrefixExpression *E){
    llvm::Value *Result = emitExpr(E->getExpr());
    switch (E->getOperatorInfo().getKind())
    {
    case tok::plus:
    // Identity - nothing to do.
    break;
  case tok::minus:
    Result = Builder.CreateNeg(Result);
    break;
  case tok::kw_NOT:
    Result = Builder.CreateNot(Result);
    break;
  default:
    llvm_unreachable("Wrong operator");
  }
  return Result;
}

llvm::Value *CGProcedure::emitExpr(Expr *E) {
  if (auto *Infix = llvm::dyn_cast<InfixExpression>(E)) {
    return emitInfixExpr(Infix);
  } else if (auto *Prefix =
                 llvm::dyn_cast<PrefixExpression>(E)) {
    return emitPrefixExpr(Prefix);
  } else if (auto *Var =
                 llvm::dyn_cast<VariableAccess>(E)) {
    auto *Decl = Var->getDecl();
    return readVariable(Curr, Decl);
  } else if (auto *Const =
                 llvm::dyn_cast<ConstantAccess>(E)) {
    return emitExpr(Const->getDecl()->getExpr());
  } else if (auto *IntLit =
                 llvm::dyn_cast<IntegerLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int64Ty,
                                  IntLit->getValue());
  } else if (auto *BoolLit =
                 llvm::dyn_cast<BooleanLiteral>(E)) {
    return llvm::ConstantInt::get(CGM.Int1Ty,
                                  BoolLit->getValue());
  }

  llvm::report_fatal_error("Unsupported expression");
}

void CGProcedure::emitStmt(AssignmentStatement *Stmt) {
  auto *Val = emitExpr(Stmt->getExpr());
  writeVariable(Curr, Stmt->getVar(), Val);
}

void CGProcedure::emitStmt(ProcedureCallStatement *Stmt) {
  llvm::report_fatal_error("not implemented");
}

void CGProcedure::emitStmt(IfStatement *Stmt) {
  bool HasElse = Stmt->getElseStmts().size() > 0;

  llvm::BasicBlock *IfBB = llvm::BasicBlock::Create(
      CGM.getLLVMCtx(), "if.body", Fn);
  llvm::BasicBlock *ElseBB =
      HasElse ? llvm::BasicBlock::Create(CGM.getLLVMCtx(),
                                         "else.baody", Fn)
              : nullptr;
  llvm::BasicBlock *AfterIfBB = llvm::BasicBlock::Create(
      CGM.getLLVMCtx(), "after.if", Fn);

  llvm::Value *Cond = emitExpr(Stmt->getCond());
  Builder.CreateCondBr(Cond, IfBB,
                       HasElse ? ElseBB : AfterIfBB);

  sealBlock(Curr);
  emit(Stmt->getIfStmts());
  if (!Curr->getTerminator()) {
    Builder.CreateBr(AfterIfBB);
  }
  sealBlock(Curr);

  if (HasElse) {
    setCurr(ElseBB);
    emit(Stmt->getElseStmts());
    if (!Curr->getTerminator()) {
      Builder.CreateBr(AfterIfBB);
    }
    sealBlock(Curr);
  }
  setCurr(AfterIfBB);
}

void CGProcedure::emitStmt(WhileStatement *Stmt) {
  // 条件的基本块
  llvm::BasicBlock *WhileCondBB = llvm::BasicBlock::Create(
      CGM.getLLVMCtx(), "while.cond", Fn);

  // while body 的基本块
  llvm::BasicBlock *WhileBodyBB = llvm::BasicBlock::Create(
      CGM.getLLVMCtx(), "while.body", Fn);

  // “在`while`语句之后的基本块”。
  llvm::BasicBlock *AfterWhileBB = llvm::BasicBlock::Create(
      CGM.getLLVMCtx(), "after.while", Fn);

  // 条件的基本块成为新的基本块，我们生成条件并以条件分支结束代码块
  Builder.CreateBr(WhileCondBB);
  sealBlock(Curr);
  setCurr(WhileBodyBB);
  llvm::Value *Cond = emitExpr(Stmt->getCond());
  Builder.CreateCondBr(Cond, WhileBodyBB, AfterWhileBB);

  setCurr(WhileBodyBB);
  emit(Stmt->getWhileStmts());
  Builder.CreateBr(WhileCondBB);
  sealBlock(Curr);
  sealBlock(WhileCondBB);

  setCurr(AfterWhileBB);
}

void CGProcedure::emitStmt(ReturnStatement *Stmt) {
  if (Stmt->getRetVal()) {
    llvm::Value *RetVal = emitExpr(Stmt->getRetVal());
    Builder.CreateRet(RetVal);
  } else {
    Builder.CreateRetVoid();
  }
}


void CGProcedure::run(ProcedureDeclaration *Proc) {
    this->Proc = Proc;
    Fty = createFunctionType(Proc);
    Fn = createFunction(Proc, Fty);

    llvm::BasicBlock *BB = llvm::BasicBlock::Create(CGM.getLLVMCtx(),"entry",Fn);
    setCurr(BB);

    size_t Idx = 0;

    auto &Defs = CurrentDef[BB];
    for(auto I = Fn->arg_begin(),E = Fn->arg_end(); I != E; ++I,++Idx) {
        llvm::Argument *Arg = I;
        FormalParameterDeclaration *FP = Proc->getFormalParams()[Idx];

        FormalParams[FP] = Arg;
        Defs.Defs.insert(std::pair<Decl *,llvm::Value *>(FP,Arg));
    }
    for(auto *D : Proc->getDecls()) {
        if(auto *Var = llvm::dyn_cast<VariableDeclaration>(D)) {
            llvm::Type *Ty = mapType(Var);
            if(Ty->isAggregateType()){
                llvm::Value *Val = Builder.CreateAlloca(Ty);
                Defs.Defs.insert(
                    std::pair<Decl *,llvm::Value *>(Var,Val));
            }
        }
    }

    auto Block = Proc->getStmts();
    emit(Proc->getStmts());

    if(!Curr->getTerminator()){
        Builder.CreateRetVoid();
    }
    sealBlock(Curr);
}

void CGProcedure::run(){}