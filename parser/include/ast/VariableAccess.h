#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class VariableAccess final : public ASTNode {
    public:
        explicit VariableAccess(Token name) : ASTNode(std::move(name)) {
        }

        ~VariableAccess() override = default;

        VariableAccess(VariableAccess &&) = default;

        VariableAccess(const VariableAccess &) = delete;

        VariableAccess &operator=(VariableAccess &&) = delete;

        VariableAccess &operator=(const VariableAccess &) = delete;
    };
} // ast


