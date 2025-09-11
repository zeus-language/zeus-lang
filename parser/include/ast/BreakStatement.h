#pragma once
#include "ASTNode.h"

namespace ast {
    class BreakStatement final : public ASTNode {
    public:
        explicit BreakStatement(Token breakToken) : ASTNode(std::move(breakToken)) {
        }

        ~BreakStatement() override = default;

        BreakStatement(BreakStatement &&) = default;

        BreakStatement(const BreakStatement &) = delete;

        BreakStatement &operator=(BreakStatement &&) = delete;

        BreakStatement &operator=(const BreakStatement &) = delete;
    };
}
