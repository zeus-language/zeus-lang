//
// Created by stefan on 29.08.25.
//

#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class FieldAssignment final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_accessNode;
        std::unique_ptr<ASTNode> m_expression;
        std::optional<std::shared_ptr<types::VariableType> > m_structType = std::nullopt;

    public:
        explicit FieldAssignment(Token name, std::unique_ptr<ASTNode> accessNode,
                                 std::unique_ptr<ASTNode> expression) : ASTNode(std::move(name)),
                                                                        m_accessNode(std::move(accessNode)),
                                                                        m_expression(std::move(expression)) {
        }


        [[nodiscard]] ASTNode *expression() const {
            return m_expression.get();
        }

        [[nodiscard]] ASTNode *accessNode() const {
            return m_accessNode.get();
        }

        ~FieldAssignment() override = default;

        FieldAssignment(FieldAssignment &&) = default;

        FieldAssignment(const FieldAssignment &) = delete;

        FieldAssignment &operator=(FieldAssignment &&) = delete;

        FieldAssignment &operator=(const FieldAssignment &) = delete;

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > structType() const {
            return m_structType;
        }

        void setStructType(std::shared_ptr<types::VariableType> type) {
            m_structType = std::make_optional<std::shared_ptr<types::VariableType> >(type);
        }

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_accessNode->getNodeByToken(token);
            if (result) {
                return result;
            }
            result = m_expression->getNodeByToken(token);
            if (result) {
                return result;
            }
            return std::nullopt;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<FieldAssignment>(expressionToken(),
                                                               m_accessNode->clone(),
                                                               m_expression->clone());
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            if (m_structType)
                cloneNode->setStructType(m_structType.value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_accessNode->makeNonGeneric(genericParam);
            m_expression->makeNonGeneric(genericParam);
        }
    };
} // ast


