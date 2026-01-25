#pragma once
#include "ASTNode.h"

namespace ast {
    class ContinueStatement final : public ASTNode {
    public:
        explicit ContinueStatement(Token breakToken) : ASTNode(std::move(breakToken)) {
        }

        ~ContinueStatement() override = default;

        ContinueStatement(ContinueStatement &&) = default;

        ContinueStatement(const ContinueStatement &) = delete;

        ContinueStatement &operator=(ContinueStatement &&) = delete;

        ContinueStatement &operator=(const ContinueStatement &) = delete;
        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<ContinueStatement>(expressionToken());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
}
