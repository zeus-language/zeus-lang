//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class VariableDeclaration final : public ASTNode {
    private:
        Token m_type;
        bool m_constant;
        std::optional<std::unique_ptr<ASTNode> > m_initialValue;

    public:
        explicit VariableDeclaration(Token name, Token type, bool constant,
                                     std::optional<std::unique_ptr<ASTNode> > initialValue);

        ~VariableDeclaration() override = default;

        std::optional<std::unique_ptr<ASTNode> > initialValue();

        [[nodiscard]] Token type() const;

        [[nodiscard]] bool constant() const;

        VariableDeclaration(VariableDeclaration &&) = default;

        VariableDeclaration(const VariableDeclaration &) = delete;

        VariableDeclaration &operator=(VariableDeclaration &&) = delete;

        VariableDeclaration &operator=(const VariableDeclaration &) = delete;
    };
} // ast


