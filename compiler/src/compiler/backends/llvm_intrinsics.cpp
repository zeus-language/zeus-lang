#include "llvm_intrinsics.h"

#include <vector>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/Function.h>
#include <llvm/IR/Type.h>

namespace llvm_backend {
    void createPrintfCall(llvm::LLVMContext &context, llvm::Module &module) {
        std::vector<llvm::Type *> params;
        params.push_back(llvm::PointerType::getUnqual(context));

        llvm::Type *resultType = llvm::Type::getInt32Ty(context);
        llvm::FunctionType *functionType = llvm::FunctionType::get(resultType, params, true);
        llvm::Function *function =
                llvm::Function::Create(functionType, llvm::Function::ExternalLinkage, "printf", module);
        for (auto &arg: function->args())
            arg.setName("__fmt");
    }
}
