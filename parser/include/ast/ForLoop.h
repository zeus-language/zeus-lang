#pragma once
#include "ASTNode.h"

namespace ast {
    class ForLoop final : public ASTNode {
    private:
        Token m_iterator;
        std::unique_ptr<ASTNode> m_range;
        bool m_isConstant;
        std::unique_ptr<BlockNode>  m_block;

    public:
        ForLoop(Token forToken, Token iterator, std::unique_ptr<ASTNode> rangeStart,
                const bool isConstant, std::unique_ptr<BlockNode> body) : ASTNode(std::move(forToken)),
            m_iterator(std::move(iterator)),
            m_range(std::move(rangeStart)),
            m_isConstant(isConstant),
            m_block(std::move(body)) {
        }

        ~ForLoop() override = default;

        ForLoop(ForLoop &&) = default;

        ForLoop(const ForLoop &) = delete;

        ForLoop &operator=(ForLoop &&) = delete;

        ForLoop &operator=(const ForLoop &) = delete;

        [[nodiscard]] Token iteratorToken() const { return m_iterator; }
        [[nodiscard]] ASTNode *range() const { return m_range.get(); }
        [[nodiscard]] bool isConstant() const { return m_isConstant; }
        [[nodiscard]] BlockNode* block() const { return m_block.get(); }

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_range->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            return  m_block->getNodeByToken(token);
        }

        std::unique_ptr<ASTNode> clone() override {
            auto blockClones = m_block->cloneBlock();
            auto cloneNode = std::make_unique<ForLoop>(expressionToken(),
                                                       m_iterator,
                                                       m_range->clone(),
                                                       m_isConstant,
                                                       std::move(blockClones));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_range->makeNonGeneric(genericParam);
            for (const auto &stmt: m_block->statements()) {
                stmt->makeNonGeneric(genericParam);
            }
        }
    };
}
