//
// Created by stefan on 29.08.25.
//

#include "ast/ReturnStatement.h"

namespace ast {
    ReturnStatement::ReturnStatement(Token returnToken, std::optional<std::unique_ptr<ASTNode> > returnValue)
        : ASTNode(std::move(returnToken)), m_returnValue(std::move(returnValue)) {
    }
} // ast
