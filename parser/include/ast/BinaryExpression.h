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
    enum class BinaryOperator { ADD, SUB, MUL, DIV, MOD, POW };

    class BinaryExpression final : public OperatorNode {
    private:
        BinaryOperator m_operator;


    public:
        explicit BinaryExpression(Token name, BinaryOperator op, std::unique_ptr<ASTNode> lhs,
                                  std::unique_ptr<ASTNode> rhs) : OperatorNode(std::move(name),std::move(lhs),std::move(rhs) ), m_operator(op){
        }

        ~BinaryExpression() override = default;

        [[nodiscard]] BinaryOperator binoperator() const { return m_operator; }


        [[nodiscard]]  std::string operatorFunctionName() const override {
            switch (m_operator) {
                case BinaryOperator::ADD:
                    return "__add__";
                case BinaryOperator::SUB:
                    return "__sub__";
                case BinaryOperator::MUL:
                    return "__mul__";
                case BinaryOperator::DIV:
                    return "__div__";
                case BinaryOperator::MOD:
                    return "__mod__";
                case BinaryOperator::POW:
                    return "__pow__";
            }
            return "";
        }

        BinaryExpression(BinaryExpression &&) = delete;

        BinaryExpression(const BinaryExpression &) = delete;

        BinaryExpression &operator=(BinaryExpression &&) = delete;

        BinaryExpression &operator=(const BinaryExpression &) = delete;

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


        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<BinaryExpression>(expressionToken(),
                                                                m_operator,
                                                                m_lhs->clone(),
                                                                m_rhs->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_lhs->makeNonGeneric(genericParam);
            m_rhs->makeNonGeneric(genericParam);
        }
    };
} // ast


