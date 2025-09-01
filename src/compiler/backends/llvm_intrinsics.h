#pragma once

namespace llvm {
    class LLVMContext;
    class Module;
}

namespace llvm_backend {
    struct LLVMBackendState;

    void createPrintfCall(llvm::LLVMContext &context, llvm::Module &module);
}
