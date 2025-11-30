//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class ReturnStatement final : public ASTNode {
        std::optional<std::unique_ptr<ASTNode> > m_returnValue;

    public:
        explicit ReturnStatement(Token returnToken, std::optional<std::unique_ptr<ASTNode> > returnValue);

        [[nodiscard]] std::optional<ASTNode *> returnValue() const {
            return m_returnValue.has_value() ? std::make_optional(m_returnValue.value().get()) : std::nullopt;
        }

        ~ReturnStatement() override = default;

        ReturnStatement(ReturnStatement &&) = default;

        ReturnStatement(const ReturnStatement &) = delete;

        ReturnStatement &operator=(ReturnStatement &&) = delete;

        ReturnStatement &operator=(const ReturnStatement &) = delete;

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            if (m_returnValue.has_value()) {
                return m_returnValue.value()->getNodeByToken(token);
            }
            return std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            std::optional<std::unique_ptr<ASTNode> > returnValueClone = std::nullopt;
            if (m_returnValue.has_value()) {
                returnValueClone = std::make_optional<std::unique_ptr<ASTNode> >(m_returnValue.value()->clone());
            }
            auto cloneNode = std::make_unique<ReturnStatement>(expressionToken(), std::move(returnValueClone));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            if (m_returnValue.has_value()) {
                m_returnValue.value()->makeNonGeneric(genericParam);
            }
        }
    };
} // ast


