#pragma once
#include <vector>

#include <ast/ASTNode.h>
#include "CompilerOptions.h"


namespace compiler {
    void compile(CompilerOptions options, const std::string &moduleName,
                 std::ostream &errorStream,
                 std::ostream &outputStream,
                 const std::vector<std::unique_ptr<ast::ASTNode> > &nodes);

    void parse_and_compile(const compiler::CompilerOptions &options, const std::filesystem::path &inputPath,
                           std::ostream &errorStream,
                           std::ostream &outputStream);
}
