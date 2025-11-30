//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class VariableAssignment final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_expression;

    public:
        explicit VariableAssignment(Token name, std::unique_ptr<ASTNode> expression);

        [[nodiscard]] ASTNode *expression() const {
            return m_expression.get();
        }

        ~VariableAssignment() override = default;

        VariableAssignment(VariableAssignment &&) = default;

        VariableAssignment(const VariableAssignment &) = delete;

        VariableAssignment &operator=(VariableAssignment &&) = delete;

        VariableAssignment &operator=(const VariableAssignment &) = delete;

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_expression->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            auto ownToken = expressionToken();
            return ownToken == token ? std::make_optional(const_cast<VariableAssignment *>(this)) : std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<VariableAssignment>(expressionToken(),
                                                                  m_expression->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
} // ast


