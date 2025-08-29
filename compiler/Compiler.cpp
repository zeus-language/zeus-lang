#include "Compiler.h"

#include <iostream>

#include "backends/llvm_backend.h"

namespace compiler {
    void compile(CompilerOptions options, const std::string &moduleName,
                 const std::vector<std::unique_ptr<ast::ASTNode> > &nodes) {
        llvm_backend::generateExecutable(options, moduleName, std::cerr, std::cout, options.outputDirectory.string(),
                                         nodes);
    }
}
