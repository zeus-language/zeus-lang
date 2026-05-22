//
// Created by stefan on 29.08.25.
//

#include "ast/ReturnStatement.h"

namespace ast {
    ReturnStatement::ReturnStatement(Token returnToken, const std::shared_ptr<ASTNode> &returnValue)
        : ASTNode(std::move(returnToken), NodeType::RETURN), m_returnValue(returnValue) {
    }
} // ast
