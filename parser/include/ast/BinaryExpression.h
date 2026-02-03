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
        std::optional<ast::FunctionDefinitionBase*> m_operatorFunction = std::nullopt;

    public:
        explicit BinaryExpression(Token name, BinaryOperator op, std::unique_ptr<ASTNode> lhs,
                                  std::unique_ptr<ASTNode> rhs) : ASTNode(std::move(name)), m_operator(op),
                                                                  m_lhs(std::move(lhs)), m_rhs(std::move(rhs)) {
        }

        ~BinaryExpression() override = default;

        [[nodiscard]] BinaryOperator binoperator() const { return m_operator; }
        [[nodiscard]] ASTNode *lhs() const { return m_lhs.get(); }
        [[nodiscard]] std::unique_ptr<ASTNode> movelhs() { return std::move(m_lhs); }
        [[nodiscard]] ASTNode *rhs() const { return m_rhs.get(); }
        void setLhs(std::unique_ptr<ASTNode> lhs) { m_lhs = std::move(lhs); }
        void setRhs(std::unique_ptr<ASTNode> rhs) { m_rhs = std::move(rhs);}

        [[nodiscard]]  std::string operatorFunctionName() const {
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

        BinaryExpression(BinaryExpression &&) = default;

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
        void setOperatorFunction( ast::FunctionDefinitionBase* funcDef) {
            m_operatorFunction = std::make_optional<ast::FunctionDefinitionBase*>(funcDef);
        }

        [[nodiscard]] std::optional<ast::FunctionDefinitionBase*> operatorFunction() const {
            return m_operatorFunction;
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


