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
        std::shared_ptr<ASTNode> m_returnValue;

    public:
        explicit ReturnStatement(Token returnToken, const std::shared_ptr<ASTNode> &returnValue);

        [[nodiscard]] std::optional<ASTNode *> returnValue() const {
            return m_returnValue ? std::make_optional(m_returnValue.get()) : std::nullopt;
        }

        ~ReturnStatement() override = default;

        ReturnStatement(ReturnStatement &&) = default;

        ReturnStatement(const ReturnStatement &) = delete;

        ReturnStatement &operator=(ReturnStatement &&) = delete;

        ReturnStatement &operator=(const ReturnStatement &) = delete;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            if (m_returnValue) {
                return m_returnValue->getNodeByToken(token);
            }
            return std::nullopt;
        }

        std::shared_ptr<ASTNode> clone() override {
            std::shared_ptr<ASTNode> returnValueClone = nullptr;
            if (m_returnValue) {
                returnValueClone = m_returnValue->clone();
            }
            auto cloneNode = std::make_shared<ReturnStatement>(expressionToken(), std::move(returnValueClone));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            if (m_returnValue) {
                m_returnValue->makeNonGeneric(genericParam);
            }
        }
    };
} // ast


