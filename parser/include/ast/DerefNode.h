#pragma once
#include "ASTNode.h"

namespace ast {
    class DerefNode : public ASTNode {
    private:
        std::shared_ptr<ASTNode> m_accessNode;

    public:
        DerefNode(const Token &token, std::shared_ptr<ASTNode> accessNode);

        ~DerefNode() override = default;

        DerefNode(DerefNode &&) = default;

        DerefNode(const DerefNode &other);

        [[nodiscard]] ASTNode *accessNode() const;

        std::shared_ptr<ASTNode> clone() override;
    };
}
