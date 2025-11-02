#pragma once
#include "ASTNode.h"

namespace ast {
    class ForLoop final : public ASTNode {
    private:
        Token m_iterator;
        std::unique_ptr<ASTNode> m_range;
        bool m_isConstant;
        std::vector<std::unique_ptr<ASTNode> > m_block;

    public:
        ForLoop(Token forToken, Token iterator, std::unique_ptr<ASTNode> rangeStart,
                const bool isConstant, std::vector<std::unique_ptr<ASTNode> > body) : ASTNode(std::move(forToken)),
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
        [[nodiscard]] const std::vector<std::unique_ptr<ASTNode> > &block() const { return m_block; }

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_range->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            for (auto &stmt: m_block) {
                result = stmt->getNodeByToken(token);
                if (result.has_value()) {
                    return result;
                }
            }
            return std::nullopt;
        }
    };
}
