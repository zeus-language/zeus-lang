#pragma once

#include <memory>
#include <optional>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class FieldAccess final : public ASTNode {
    private:
        Token m_fieldName;

        std::optional<std::shared_ptr<types::VariableType> > m_structType = std::nullopt;

    public:
        explicit FieldAccess(Token name, Token fieldName) : ASTNode(std::move(name)),
                                                            m_fieldName(std::move(fieldName)) {
        }

        [[nodiscard]] Token fieldName() const {
            return m_fieldName;
        }

        ~FieldAccess() override = default;

        FieldAccess(FieldAccess &&) = default;

        FieldAccess(const FieldAccess &) = delete;

        FieldAccess &operator=(FieldAccess &&) = delete;

        FieldAccess &operator=(const FieldAccess &) = delete;

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > structType() const {
            return m_structType;
        }

        void setStructType(std::shared_ptr<types::VariableType> type) {
            m_structType = std::make_optional<std::shared_ptr<types::VariableType> >(type);
        }
    };
} // ast


