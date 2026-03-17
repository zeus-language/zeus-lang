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

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            if (expressionToken() == token) {
                return const_cast<DeferStatement *>(this);
            }
            if (m_deferredNode) {
                if (auto result = m_deferredNode->getNodeByToken(token)) {
                    return result;
                }
            }
            return std::nullopt;
        }
    };
}
