#pragma once

#include "ASTNode.h"

namespace ast {
    class DeferStatement : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_deferredNode;

    public:
        explicit DeferStatement(Token token, std::unique_ptr<ASTNode> deferredNode) : ASTNode(std::move(token)),
                                                                                       m_deferredNode(std::move(deferredNode)) {
        }

        ~DeferStatement() override = default;

        [[nodiscard]] ASTNode *deferredNode() const { return m_deferredNode.get(); }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<DeferStatement>(expressionToken(), m_deferredNode->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }
    };
}
