#include "ast/VariableAssignment.h"

namespace ast {
    VariableAssignment::VariableAssignment(Token name, std::unique_ptr<ASTNode> expression) : ASTNode(std::move(
            name)), m_expression(std::move(expression)) {
    }
} // ast
