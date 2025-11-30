//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    enum class LogicalOperator { AND, OR, XOR, NOT };

    class LogicalExpression final : public ASTNode {
    private:
        LogicalOperator m_operator;
        std::unique_ptr<ASTNode> m_lhs;
        std::unique_ptr<ASTNode> m_rhs;

    public:
        explicit LogicalExpression(Token name, LogicalOperator op, std::unique_ptr<ASTNode> lhs,
                                   std::unique_ptr<ASTNode> rhs) : ASTNode(std::move(name)), m_operator(op),
                                                                   m_lhs(std::move(lhs)), m_rhs(std::move(rhs)) {
        }

        explicit LogicalExpression(Token name, LogicalOperator op,
                                   std::unique_ptr<ASTNode> rhs) : ASTNode(std::move(name)), m_operator(op),
                                                                   m_lhs(nullptr), m_rhs(std::move(rhs)) {
        }

        [[nodiscard]] LogicalOperator logical_operator() const {
            return m_operator;
        }

        [[nodiscard]] ASTNode *lhs() const { return m_lhs.get(); }

        [[nodiscard]] ASTNode *rhs() const { return m_rhs.get(); }

        ~LogicalExpression() override = default;

        LogicalExpression(LogicalExpression &&) = default;

        LogicalExpression(const LogicalExpression &) = delete;

        LogicalExpression &operator=(LogicalExpression &&) = delete;

        LogicalExpression &operator=(const LogicalExpression &) = delete;

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_lhs ? m_lhs->getNodeByToken(token) : std::nullopt;
            if (result) {
                return result;
            }
            result = m_rhs->getNodeByToken(token);
            if (result) {
                return result;
            }
            return std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            std::unique_ptr<ASTNode> lhsClone = nullptr;
            if (m_lhs) {
                lhsClone = m_lhs->clone();
            }
            auto cloneNode = std::make_unique<LogicalExpression>(expressionToken(),
                                                                 m_operator,
                                                                 std::move(lhsClone),
                                                                 m_rhs->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
} // ast


