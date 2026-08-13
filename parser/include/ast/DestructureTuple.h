#pragma once

#include "ASTNode.h"

namespace ast {
    class DestructureTuple : public ast::ASTNode {
        Token m_variantName;
        std::vector<Token> m_memberNames;
        ast::ASTNode *m_accessNode = nullptr;

    public:
        explicit DestructureTuple(const Token &token, const Token &variantName, const std::vector<Token> &memberNames)
            : ASTNode(token, NodeType::DESTRUCTURE_TUPLE), m_variantName(variantName), m_memberNames(memberNames) {
        }

        ~DestructureTuple() override = default;

        DestructureTuple(DestructureTuple &&) = default;

        DestructureTuple(const DestructureTuple &) = delete;

        DestructureTuple &operator=(DestructureTuple &&) = delete;

        DestructureTuple &operator=(const DestructureTuple &) = delete;

        [[nodiscard]] Token variantName() const {
            return m_variantName;
        }


        [[nodiscard]] const std::vector<Token> &memberNames() const {
            return m_memberNames;
        }

        void setAccessNode(ast::ASTNode *accessNode) {
            m_accessNode = accessNode;
        }

        [[nodiscard]] ast::ASTNode *accessNode() const {
            return m_accessNode;
        }

        std::shared_ptr<ASTNode> clone() override {
            auto members = std::vector(m_memberNames);
            auto cloneNode = std::make_shared<DestructureTuple>(expressionToken(), m_variantName, members);
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }
    };
} // ast
