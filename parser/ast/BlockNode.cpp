//
// Created by stefan on 03.03.26.
//

#include "ast/BlockNode.h"

#include "ast/ForLoop.h"
#include "ast/IfCondition.h"
#include "ast/VariableDeclaration.h"
#include "ast/WhileLoop.h"

namespace ast {
    BlockNode::BlockNode(const Token &token, std::vector<std::unique_ptr<ASTNode> > statements)
        : ASTNode(token), m_statements(std::move(statements)) {
    }

    const std::vector<std::unique_ptr<ASTNode> > &BlockNode::statements() {
        return m_statements;
    }

    std::unique_ptr<BlockNode> BlockNode::cloneBlock() const {
        std::vector<std::unique_ptr<ASTNode> > statementsClone;
        statementsClone.reserve(m_statements.size());
        for (const auto &statement: m_statements) {
            statementsClone.push_back(statement->clone());
        }
        auto cloneNode = std::make_unique<BlockNode>(expressionToken(), std::move(statementsClone));
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return std::move(cloneNode);
    }

    std::unique_ptr<ASTNode> BlockNode::clone() {
        return cloneBlock();
    }


    std::optional<ASTNode *> BlockNode::getNodeByToken(const Token &token) const {
        for (auto &stmt: m_statements) {
            if (const auto node = stmt->getNodeByToken(token)) {
                return node;
            }
        }
        return std::nullopt;
    }

    std::optional<ASTNode *> BlockNode::getVariableDefinition(const std::string &name) const {
        for (const auto &stmt: m_statements) {
            if (auto decl = dynamic_cast<VariableDeclaration *>(stmt.get())) {
                if (decl->expressionToken().lexical() == name) {
                    return std::make_optional<ASTNode *>(decl);
                }
            } else if (auto forLoop = dynamic_cast<ForLoop *>(stmt.get())) {
                if (forLoop->iteratorToken().lexical() == name) {
                    return std::make_optional<ASTNode *>(forLoop);
                }
                if (const auto result = forLoop->block()->getVariableDefinition(name)) {
                    return result;
                }
            } else if (const auto block = dynamic_cast<BlockNode *>(stmt.get())) {
                if (const auto result = block->getVariableDefinition(name)) {
                    return result;
                }
            } else if (const auto whileLoop = dynamic_cast<WhileLoop *>(stmt.get())) {
                if (const auto result = whileLoop->block()->getVariableDefinition(name)) {
                    return result;
                }
            } else if (const auto ifCond = dynamic_cast<IfCondition *>(stmt.get())) {
                if (const auto result = ifCond->ifBlock()->getVariableDefinition(name)) {
                    return result;
                }
                if (const auto elseBlock = ifCond->elseBlock()) {
                    if (const auto result = elseBlock.value()->getVariableDefinition(name)) {
                        return result;
                    }
                }
            }

            // TODO nested check
        }
        return std::nullopt;
    }
} // namespace ast
