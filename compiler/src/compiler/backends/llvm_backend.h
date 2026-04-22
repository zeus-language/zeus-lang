
#pragma once
#include <string>
#include <vector>

#include "ast/ASTNode.h"
#include "../CompilerOptions.h"
#include "parser/Parser.h"

namespace llvm_backend {
    void initializeLLVMBackend();

    void generateExecutable(const compiler::CompilerOptions &options, const std::string &moduleName,
                            std::ostream &errorStream,
                            std::ostream &outputStream, const std::shared_ptr<parser::Module> &module,
                            const std::vector<std::shared_ptr<types::VariableType> > &registeredTypes);
}
