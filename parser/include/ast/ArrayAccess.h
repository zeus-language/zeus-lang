#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class ArrayAccess final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_indexExpression;

        std::optional<std::shared_ptr<types::VariableType> > m_arrayType = std::nullopt;

    public:
        explicit ArrayAccess(Token name, std::unique_ptr<ASTNode> indexExpression) : ASTNode(std::move(name)),
            m_indexExpression(std::move(indexExpression)) {
        }

        [[nodiscard]] ASTNode *index() const {
            return m_indexExpression.get();
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
    };
} // ast


