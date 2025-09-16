#include "ast/VariableDeclaration.h"

namespace ast {
    VariableDeclaration::VariableDeclaration(Token name, std::unique_ptr<RawType> type, bool constant,
                                             std::optional<std::unique_ptr<ASTNode> > initialValue) : ASTNode(std::move(
            name)), m_type(std::move(type)),
        m_initialValue(std::move(initialValue)), m_constant(constant) {
    }

    std::optional<ASTNode *> VariableDeclaration::initialValue() const {
        return m_initialValue.has_value() ? std::make_optional<ASTNode *>(m_initialValue->get()) : std::nullopt;
    }

    RawType *VariableDeclaration::type() const {
        return m_type.get();
    }

    bool VariableDeclaration::constant() const {
        return m_constant;
    }
} // ast
