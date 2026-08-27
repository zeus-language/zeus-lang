//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "OperatorNode.h"
#include "lexer/Lexer.h"

namespace ast {
    enum class BinaryAssignmentOperator {
        ADD, SUB, MUL, DIV, MOD, POW, AND, OR, LEFT_SHIFT, RIGHT_SHIFT, NOT, UNARY_MINUS, XOR
    };

    class BinaryAssignmentExpression final : public OperatorNode {
    private:
        BinaryAssignmentOperator m_operator;

    public:
        explicit BinaryAssignmentExpression(Token name, BinaryAssignmentOperator op, std::shared_ptr<ASTNode> lhs,
                                            std::shared_ptr<ASTNode> rhs) : OperatorNode(std::move(name),
                                                                                NodeType::BINARY_ASSIGN_EXPRESSION,
                                                                                std::move(lhs), std::move(rhs)),
                                                                            m_operator(op) {
        }

        ~BinaryAssignmentExpression() override = default;

        [[nodiscard]] BinaryAssignmentOperator binoperator() const { return m_operator; }


        [[nodiscard]] std::string operatorFunctionName() const override {
            switch (m_operator) {
                case BinaryAssignmentOperator::ADD:
                    return "__add_assign__";
                case BinaryAssignmentOperator::SUB:
                    return "__sub_assign__";
                case BinaryAssignmentOperator::MUL:
                    return "__mul_assign__";
                case BinaryAssignmentOperator::DIV:
                    return "__div_assign__";
                case BinaryAssignmentOperator::MOD:
                    return "__mod_assign__";
                case BinaryAssignmentOperator::POW:
                    return "__pow_assign__";
                case BinaryAssignmentOperator::AND:
                    return "__and_assign__";
                case BinaryAssignmentOperator::OR:
                    return "__or_assign__";
                case BinaryAssignmentOperator::LEFT_SHIFT:
                    return "__lshift_assign__";
                case BinaryAssignmentOperator::RIGHT_SHIFT:
                    return "__rshift_assign__";
                case BinaryAssignmentOperator::NOT:
                    break;
                case BinaryAssignmentOperator::UNARY_MINUS:
                    break;
                case BinaryAssignmentOperator::XOR:
                    break;
            }
            return "";
        }

        BinaryAssignmentExpression(BinaryAssignmentExpression &&) = delete;

        BinaryAssignmentExpression(const BinaryAssignmentExpression &) = delete;

        BinaryAssignmentExpression &operator=(BinaryAssignmentExpression &&) = delete;

        BinaryAssignmentExpression &operator=(const BinaryAssignmentExpression &) = delete;

        [[nodiscard]] bool constant() const override {
            return m_lhs->constant() && m_rhs->constant();
        }


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


        std::shared_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_shared<BinaryAssignmentExpression>(expressionToken(),
                                                                          m_operator,
                                                                          m_lhs->clone(),
                                                                          m_rhs->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_lhs->makeNonGeneric(genericParam);
            m_rhs->makeNonGeneric(genericParam);
        }
    };
} // ast
