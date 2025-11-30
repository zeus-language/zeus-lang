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

        [[nodiscard]] bool constant() const override {
            return m_accessNode->constant();
        }

        ~ReferenceAccess() override = default;

        ReferenceAccess(ReferenceAccess &&) = default;

        ReferenceAccess(const ReferenceAccess &) = delete;

        ReferenceAccess &operator=(ReferenceAccess &&) = delete;

        ReferenceAccess &operator=(const ReferenceAccess &) = delete;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_accessNode->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            auto ownToken = expressionToken();
            return ownToken == token ? std::make_optional(const_cast<ReferenceAccess *>(this)) : std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<ReferenceAccess>(expressionToken(),
                                                               m_accessNode->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_accessNode->makeNonGeneric(genericParam);
        }
    };
} // ast


