//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    enum class BinaryOperator { ADD, SUB, MUL, DIV, MOD, POW };

    class BinaryExpression final : public ASTNode {
    private:
        BinaryOperator m_operator;
        std::unique_ptr<ASTNode> m_lhs;
        std::unique_ptr<ASTNode> m_rhs;

    public:
        explicit BinaryExpression(Token name, BinaryOperator op, std::unique_ptr<ASTNode> lhs,
                                  std::unique_ptr<ASTNode> rhs) : ASTNode(std::move(name)), m_operator(op),
                                                                  m_lhs(std::move(lhs)), m_rhs(std::move(rhs)) {
        }

        ~BinaryExpression() override = default;

        [[nodiscard]] BinaryOperator binoperator() const { return m_operator; }

        std::unique_ptr<ASTNode> lhs() { return std::move(m_lhs); }
        std::unique_ptr<ASTNode> rhs() { return std::move(m_rhs); }

        BinaryExpression(BinaryExpression &&) = default;

        BinaryExpression(const BinaryExpression &) = delete;

        BinaryExpression &operator=(BinaryExpression &&) = delete;

        BinaryExpression &operator=(const BinaryExpression &) = delete;
    };
} // ast


