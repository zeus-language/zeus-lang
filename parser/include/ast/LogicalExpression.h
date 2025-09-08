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
    };
} // ast


