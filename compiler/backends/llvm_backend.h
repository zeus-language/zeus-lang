
#pragma once
#include <string>
#include <vector>

#include "ast/ASTNode.h"
#include "../CompilerOptions.h"

namespace llvm_backend {
    void initializeLLVMBackend();

    void generateExecutable(const compiler::CompilerOptions &options, const std::string &moduleName,
                            std::ostream &errorStream,
                            std::ostream &outputStream,
                            const std::string &outputPath, const std::vector<std::unique_ptr<ast::ASTNode> > &nodes);
}
