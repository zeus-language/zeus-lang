//
// Created by stefan on 03.03.26.
//

#include "ast/BlockNode.h"

namespace ast
{
    BlockNode::BlockNode(const Token &token, std::vector<std::unique_ptr<ASTNode>> statements)
        : ASTNode(token), m_statements(std::move(statements))
    {

    }
    const std::vector<std::unique_ptr<ASTNode>> &BlockNode::statements()
    {
        return m_statements;
    }
    std::unique_ptr<BlockNode> BlockNode::cloneBlock() const
    {
        std::vector<std::unique_ptr<ASTNode>> statementsClone;
        for (const auto &statement: m_statements) {
            statementsClone.push_back(statement->clone());
        }
        auto cloneNode = std::make_unique<BlockNode>(expressionToken(), std::move(statementsClone));
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return cloneNode;
    }
    std::unique_ptr<ASTNode> BlockNode::clone()
    {
        return cloneBlock();
    }
} // namespace ast
