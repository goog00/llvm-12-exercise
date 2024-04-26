#include "tinylang/CodeGen/CGModule.h"
#include "tinylang/CodeGen/CGProcedure.h"
#include "llvm/ADT/StringExtras.h"


using namespace tinylang;

void CGModule::initialize(){
    VoidTy = llvm::Type::getVoidTy(getLLVMCtx());
    Int1Ty = llvm::Type::getInt1Ty(getLLVMCtx());
    Int32Ty = llvm::Type::getInt32Ty(getLLVMCtx());
    Int64Ty = llvm::Type::getInt64Ty(getLLVMCtx());
    Int32Zero = llvm::ConstantInt::get(Int32Ty,0,true);

} 



llvm::Type *CGModule::convertType(TypeDeclaration *Ty) {
    if(Ty->getName() == "INTEGER")
       return Int64Ty;
    if(Ty->getName() == "BOOLEAN")
       return Int1Ty;
    llvm::report_fatal_error("Unsupported type");      
}

std::string CGModule::mangleName(Decl *D){
    std::string Mangled;
    llvm::SmallString<16> Tmp;
    while (D)
    {
        llvm::StringRef Name = D->getName();
        Tmp.clear();
        Tmp.append(llvm::itostr(Name.size()));
        Tmp.append(Name);
        Mangled.insert(0,Tmp.c_str());
        D = D->getEnclosingDecl();
    }

    Mangled.insert(0,"_t");
    return Mangled;
    
}

