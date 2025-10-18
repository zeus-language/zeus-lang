#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class ReferenceAccess final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_accessNode;

    public:
        explicit ReferenceAccess(Token name, std::unique_ptr<ASTNode> accessNode) : ASTNode(std::move(name)),
            m_accessNode(std::move(accessNode)) {
        }

        [[nodiscard]] Token fieldName() const {
            return expressionToken();
        }

        [[nodiscard]] ASTNode *accessNode() const {
            return m_accessNode.get();
        }

        ~ReferenceAccess() override = default;

        ReferenceAccess(ReferenceAccess &&) = default;

        ReferenceAccess(const ReferenceAccess &) = delete;

        ReferenceAccess &operator=(ReferenceAccess &&) = delete;

        ReferenceAccess &operator=(const ReferenceAccess &) = delete;
    };
} // ast


