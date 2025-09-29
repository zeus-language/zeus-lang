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
        Token m_fieldName;
        std::unique_ptr<ASTNode> m_expression;
        std::optional<std::shared_ptr<types::VariableType> > m_structType = std::nullopt;

    public:
        explicit FieldAssignment(Token name, Token fieldName,
                                 std::unique_ptr<ASTNode> expression) : ASTNode(std::move(name)),
                                                                        m_fieldName(std::move(fieldName)),
                                                                        m_expression(std::move(expression)) {
        }


        [[nodiscard]] ASTNode *expression() const {
            return m_expression.get();
        }

        [[nodiscard]] Token fieldName() const {
            return m_fieldName;
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


