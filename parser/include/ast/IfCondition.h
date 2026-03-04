#pragma once
#include <memory>

#include "ASTNode.h"

namespace ast {
    class IfCondition final : public ASTNode {
        std::unique_ptr<ASTNode> m_condition;
        std::unique_ptr<BlockNode>  m_ifBlock;
        std::optional<std::unique_ptr<BlockNode>>  m_elseBlock;

    public:
        IfCondition(Token token, std::unique_ptr<ASTNode> condition, std::unique_ptr<BlockNode>  ifBlock,
                     std::optional<std::unique_ptr<BlockNode>> elseBlock) : ASTNode(std::move(token)),
                                                                        m_condition(std::move(condition)),
                                                                        m_ifBlock(std::move(ifBlock)),
                                                                        m_elseBlock(std::move(elseBlock)) {
        }

        ~IfCondition() override = default;

        [[nodiscard]] ASTNode *condition() const { return m_condition.get(); }

        [[nodiscard]] BlockNode* ifBlock() { return m_ifBlock.get(); }

        [[nodiscard]] std::optional<BlockNode*> elseBlock()
        {
            return m_elseBlock.has_value() ? std::make_optional<BlockNode*>(m_elseBlock->get()) : std::nullopt;
        }

        IfCondition(IfCondition &&) = default;

        IfCondition(const IfCondition &) = delete;

        IfCondition &operator=(IfCondition &&) = delete;

        IfCondition &operator=(const IfCondition &) = delete;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_condition->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            result = m_ifBlock->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            if (m_elseBlock) {
                result = m_elseBlock.value()->getNodeByToken(token);
                if (result.has_value()) {
                    return result;
                }
            }
            return std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto ifBlock = m_ifBlock->cloneBlock();

            auto elseBlock = m_elseBlock.has_value() ? std::make_optional<std::unique_ptr<BlockNode>>(m_elseBlock.value()->cloneBlock()) : std::nullopt;
            auto conditionClone = m_condition->clone();
            auto cloneNode = std::make_unique<IfCondition>(expressionToken(), std::move(conditionClone),
                                                           std::move(ifBlock), std::move(elseBlock));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_condition->makeNonGeneric(genericParam);
            for (auto &stmt: m_ifBlock->statements()) {
                stmt->makeNonGeneric(genericParam);
            }
            if (m_elseBlock) {
                for (auto &stmt: m_elseBlock.value()->statements()) {
                    stmt->makeNonGeneric(genericParam);
                }
            }
        }
    };
}
