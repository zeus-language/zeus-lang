#pragma once


#include <memory>
#include <optional>

#include "ASTNode.h"
#include "OperatorNode.h"
#include "lexer/Lexer.h"

namespace ast {
    enum class CMPOperator { GREATER, LESS, GREATER_EQUAL, LESS_EQUAL, EQUALS, NOT_EQUALS };

    class Comparisson final : public OperatorNode {
    private:
        CMPOperator m_operator;
    public:
        explicit Comparisson(Token name, CMPOperator op, std::unique_ptr<ASTNode> lhs,
                             std::unique_ptr<ASTNode> rhs) : OperatorNode(std::move(name),std::move(lhs),std::move(rhs)
                                 ), m_operator(op)
                                                              {
        }

        ~Comparisson() override = default;

        [[nodiscard]] CMPOperator cmpoperator() const { return m_operator; }


        [[nodiscard]]  std::string operatorFunctionName() const override {
            switch (m_operator) {
                case CMPOperator::GREATER:
                    return "__gt__";
                case CMPOperator::LESS:
                    return "__lt__";
                case CMPOperator::GREATER_EQUAL:
                    return "__ge__";
                case CMPOperator::LESS_EQUAL:
                    return "__le__";
                case CMPOperator::EQUALS:
                    return "__eq__";
                case CMPOperator::NOT_EQUALS:
                    return "__ne__";
            }
            return "";
        }

        Comparisson(Comparisson &&) = delete;

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


