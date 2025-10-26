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
    };
} // ast


