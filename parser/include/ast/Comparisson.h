#pragma once


#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    enum class CMPOperator { GREATER, LESS, GREATER_EQUAL, LESS_EQUAL, EQUALS, NOT_EQUALS };

    class Comparisson final : public ASTNode {
    private:
        CMPOperator m_operator;
        std::unique_ptr<ASTNode> m_lhs;
        std::unique_ptr<ASTNode> m_rhs;

    public:
        explicit Comparisson(Token name, CMPOperator op, std::unique_ptr<ASTNode> lhs,
                             std::unique_ptr<ASTNode> rhs) : ASTNode(std::move(name)), m_operator(op),
                                                             m_lhs(std::move(lhs)), m_rhs(std::move(rhs)) {
        }

        ~Comparisson() override = default;

        [[nodiscard]] CMPOperator cmpoperator() const { return m_operator; }
        [[nodiscard]] ASTNode *lhs() const { return m_lhs.get(); }
        [[nodiscard]] std::unique_ptr<ASTNode> movelhs() { return std::move(m_lhs); }
        [[nodiscard]] ASTNode *rhs() const { return m_rhs.get(); }

        Comparisson(Comparisson &&) = default;

        Comparisson(const Comparisson &) = delete;

        Comparisson &operator=(Comparisson &&) = delete;

        Comparisson &operator=(const Comparisson &) = delete;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_lhs->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            result = m_rhs->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            return std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<Comparisson>(expressionToken(),
                                                           m_operator,
                                                           m_lhs->clone(),
                                                           m_rhs->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
} // ast


