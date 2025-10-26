#pragma once
#include "ASTNode.h"

namespace ast {
    class ForLoop final : public ASTNode {
    private:
        Token m_iterator;
        std::unique_ptr<ASTNode> m_rangeStart;
        std::unique_ptr<ASTNode> m_rangeEnd;
        bool m_isConstant;
        bool m_inclusive;
        std::vector<std::unique_ptr<ASTNode> > m_block;

    public:
        ForLoop(Token forToken, Token iterator, std::unique_ptr<ASTNode> rangeStart, std::unique_ptr<ASTNode> rangeEnd,
                const bool isConstant, const bool inclusive,
                std::vector<std::unique_ptr<ASTNode> > body) : ASTNode(std::move(forToken)),
                                                               m_iterator(std::move(iterator)),
                                                               m_rangeStart(std::move(rangeStart)),
                                                               m_rangeEnd(std::move(rangeEnd)),
                                                               m_isConstant(isConstant),
                                                               m_inclusive(inclusive),
                                                               m_block(std::move(body)) {
        }

        ~ForLoop() override = default;

        ForLoop(ForLoop &&) = default;

        ForLoop(const ForLoop &) = delete;

        ForLoop &operator=(ForLoop &&) = delete;

        ForLoop &operator=(const ForLoop &) = delete;

        [[nodiscard]] Token iteratorToken() const { return m_iterator; }
        [[nodiscard]] ASTNode *rangeStart() const { return m_rangeStart.get(); }
        [[nodiscard]] ASTNode *rangeEnd() const { return m_rangeEnd.get(); }
        [[nodiscard]] bool inclusive() const { return m_inclusive; }
        [[nodiscard]] bool isConstant() const { return m_isConstant; }
        [[nodiscard]] const std::vector<std::unique_ptr<ASTNode> > &block() const { return m_block; }

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_rangeStart->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            result = m_rangeEnd->getNodeByToken(token);
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
