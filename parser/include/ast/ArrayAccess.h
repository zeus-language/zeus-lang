#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class ArrayAccess final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_accessExpression;
        std::unique_ptr<ASTNode> m_indexExpression;

        std::optional<std::shared_ptr<types::VariableType> > m_arrayType = std::nullopt;

    public:
        explicit ArrayAccess(Token name, std::unique_ptr<ASTNode> accessExpression,
                             std::unique_ptr<ASTNode> indexExpression) : ASTNode(std::move(name)),
                                                                         m_accessExpression(
                                                                             std::move(accessExpression)),
                                                                         m_indexExpression(std::move(indexExpression)) {
        }

        [[nodiscard]] ASTNode *index() const {
            return m_indexExpression.get();
        }

        [[nodiscard]] ASTNode *accessExpression() const {
            return m_accessExpression.get();
        }

        ~ArrayAccess() override = default;

        ArrayAccess(ArrayAccess &&) = default;

        ArrayAccess(const ArrayAccess &) = delete;

        ArrayAccess &operator=(ArrayAccess &&) = delete;

        ArrayAccess &operator=(const ArrayAccess &) = delete;

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > arrayType() const {
            return m_arrayType;
        }

        void setArrayType(std::shared_ptr<types::VariableType> type) {
            m_arrayType = std::make_optional<std::shared_ptr<types::VariableType> >(type);
        }

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_accessExpression->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            result = m_indexExpression->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            auto ownToken = expressionToken();
            return ownToken == token ? std::make_optional(const_cast<ArrayAccess *>(this)) : std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<ArrayAccess>(expressionToken(),
                                                           m_accessExpression->clone(),
                                                           m_indexExpression->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            if (m_arrayType)
                cloneNode->setArrayType(m_arrayType.value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_accessExpression->makeNonGeneric(genericParam);
            m_indexExpression->makeNonGeneric(genericParam);
            m_arrayType = m_arrayType.value()->makeNonGenericType(genericParam);
        }
    };
} // ast


