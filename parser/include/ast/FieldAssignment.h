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
    };
} // ast


