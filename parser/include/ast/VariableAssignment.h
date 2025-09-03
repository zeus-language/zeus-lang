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

        std::unique_ptr<ASTNode> expression() {
            return std::move(m_expression);
        }

        ~VariableAssignment() override = default;

        VariableAssignment(VariableAssignment &&) = default;

        VariableAssignment(const VariableAssignment &) = delete;

        VariableAssignment &operator=(VariableAssignment &&) = delete;

        VariableAssignment &operator=(const VariableAssignment &) = delete;
    };
} // ast


