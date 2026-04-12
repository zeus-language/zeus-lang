#pragma once
#include "Scope.h"
#include "ast/BlockNode.h"
#include "ast/VariableDeclaration.h"

namespace types {
    std::optional<std::unique_ptr<ast::RawType> > resolveRawTypeFromUsage(ast::BlockNode *blockNode,
                                                                          const std::string &typeName);

    std::optional<std::unique_ptr<ast::RawType> > resolveRawReturnTypeFromUsage(ast::BlockNode *node);

    std::optional<std::shared_ptr<VariableType> > resolveFromRawType(ast::RawType *rawType,
                                                                     std::shared_ptr<Scope> &currentScope,
                                                                     bool resolveGeneric = false);
}
