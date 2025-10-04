#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class FieldAccess final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_accessNode;

        std::optional<std::shared_ptr<types::VariableType> > m_structType = std::nullopt;

    public:
        explicit FieldAccess(Token name, std::unique_ptr<ASTNode> accessNode) : ASTNode(std::move(name)),
            m_accessNode(std::move(accessNode)) {
        }

        [[nodiscard]] Token fieldName() const {
            return expressionToken();
        }

        [[nodiscard]] ASTNode *accessNode() const {
            return m_accessNode.get();
        }

        ~FieldAccess() override = default;

        FieldAccess(FieldAccess &&) = default;

        FieldAccess(const FieldAccess &) = delete;

        FieldAccess &operator=(FieldAccess &&) = delete;

        FieldAccess &operator=(const FieldAccess &) = delete;

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > structType() const {
            return m_structType;
        }

        void setStructType(std::shared_ptr<types::VariableType> type) {
            m_structType = std::make_optional<std::shared_ptr<types::VariableType> >(type);
        }
    };
} // ast


