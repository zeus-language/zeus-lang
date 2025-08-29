#pragma once
#include <vector>

#include "ast/ASTNode.h"
#include "CompilerOptions.h"


namespace compiler {
    void compile(CompilerOptions options, const std::string &moduleName,
                 const std::vector<std::unique_ptr<ast::ASTNode> > &nodes);
}
