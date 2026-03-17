#include "ast/VariableAssignment.h"

namespace ast {
    VariableAssignment::VariableAssignment(Token name, std::unique_ptr<ASTNode> expression) : ASTNode(std::move(
                name), NodeType::VARIABLE_ASSIGNMENT), m_expression(std::move(expression)) {
    }

    std::optional<ASTNode *> VariableAssignment::getNodeByToken(const Token &token) const {
        if (const auto result = m_expression->getNodeByToken(token)) {
            return result;
        }
        return ASTNode::getNodeByToken(token);
    }

    std::unique_ptr<ASTNode> VariableAssignment::clone() {
        auto cloneNode = std::make_unique<VariableAssignment>(expressionToken(),
                                                              m_expression->clone());
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return std::move(cloneNode);
    }
} // ast
