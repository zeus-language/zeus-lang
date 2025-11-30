//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class ArrayAssignment final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_accessNode;
        std::unique_ptr<ASTNode> m_value;
        std::unique_ptr<ASTNode> m_index;
        std::shared_ptr<types::VariableType> m_arrayType = nullptr;

    public:
        explicit ArrayAssignment(Token name, std::unique_ptr<ASTNode> accessNode, std::unique_ptr<ASTNode> expression,
                                 std::unique_ptr<ASTNode> index)
            : ASTNode(std::move(name)), m_accessNode(std::move(accessNode)), m_value(std::move(expression)),
              m_index(std::move(index)) {
        }

        [[nodiscard]] ASTNode *index() const {
            return m_index.get();
        }

        [[nodiscard]] ASTNode *value() const {
            return m_value.get();
        }

        [[nodiscard]] ASTNode *accessNode() const {
            return m_accessNode.get();
        }

        ~ArrayAssignment() override = default;

        [[nodiscard]] Token arrayToken() const {
            return expressionToken();
        }

        void setArrayType(const std::shared_ptr<types::VariableType> &arrayType) {
            m_arrayType = arrayType;
        }

        [[nodiscard]] std::shared_ptr<types::VariableType> arrayType() const {
            return m_arrayType;
        }

        ArrayAssignment(ArrayAssignment &&) = default;

        ArrayAssignment(const ArrayAssignment &) = delete;

        ArrayAssignment &operator=(ArrayAssignment &&) = delete;

        ArrayAssignment &operator=(const ArrayAssignment &) = delete;

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_accessNode->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            result = m_value->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            result = m_index->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            auto ownToken = expressionToken();
            return ownToken == token ? std::make_optional(const_cast<ArrayAssignment *>(this)) : std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<ArrayAssignment>(expressionToken(),
                                                               m_accessNode->clone(),
                                                               m_value->clone(),
                                                               m_index->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            if (m_arrayType)
                cloneNode->setArrayType(m_arrayType);
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_accessNode->makeNonGeneric(genericParam);
            m_value->makeNonGeneric(genericParam);
            m_index->makeNonGeneric(genericParam);
            m_arrayType = m_arrayType->makeNonGenericType(genericParam);
        }
    };
} // ast


