#pragma once

#include "ASTNode.h"

namespace ast {
    class DestructureStruct : public ast::ASTNode {
        Token m_variantName;
        std::vector<Token> m_memberNames;
        ast::ASTNode *m_accessNode = nullptr;

    public:
        explicit DestructureStruct(const Token &token, const Token &variantName, const std::vector<Token> &memberNames)
            : ASTNode(token, NodeType::DESTRUCTURE_STRUCT), m_variantName(variantName), m_memberNames(memberNames) {
        }

        ~DestructureStruct() override = default;

        DestructureStruct(DestructureStruct &&) = default;

        DestructureStruct(const DestructureStruct &) = delete;

        DestructureStruct &operator=(DestructureStruct &&) = delete;

        DestructureStruct &operator=(const DestructureStruct &) = delete;

        [[nodiscard]] Token variantName() const {
            return m_variantName;
        }

        void setAccessNode(ast::ASTNode *accessNode) {
            m_accessNode = accessNode;
        }

        [[nodiscard]] ast::ASTNode *accessNode() const {
            return m_accessNode;
        }

        [[nodiscard]] const std::vector<Token> &memberNames() const {
            return m_memberNames;
        }

        std::shared_ptr<ASTNode> clone() override {
            auto members = std::vector(m_memberNames);
            auto cloneNode = std::make_shared<DestructureStruct>(expressionToken(), m_variantName, members);
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            cloneNode->setAccessNode(m_accessNode);
            return std::move(cloneNode);
        }
    };
} // ast
