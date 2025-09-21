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
        std::unique_ptr<ASTNode> m_value;
        std::unique_ptr<ASTNode> m_index;
        std::shared_ptr<types::VariableType> m_arrayType = nullptr;

    public:
        explicit ArrayAssignment(Token name, std::unique_ptr<ASTNode> expression,
                                 std::unique_ptr<ASTNode> index)
            : ASTNode(std::move(name)), m_value(std::move(expression)), m_index(std::move(index)) {
        }

        [[nodiscard]] ASTNode *index() const {
            return m_index.get();
        }

        [[nodiscard]] ASTNode *value() const {
            return m_value.get();
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
    };
} // ast


