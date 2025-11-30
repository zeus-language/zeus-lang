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
 std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<BreakStatement>(expressionToken());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
}
