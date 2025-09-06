#include "ast/VariableDeclaration.h"

namespace ast {
    VariableDeclaration::VariableDeclaration(Token name, Token type, bool constant,
                                             std::optional<std::unique_ptr<ASTNode> > initialValue) : ASTNode(std::move(
            name)), m_typeToken(std::move(type)),
        m_initialValue(std::move(initialValue)), m_constant(constant) {
    }

    std::optional<ASTNode *> VariableDeclaration::initialValue() const {
        return m_initialValue.has_value() ? std::make_optional<ASTNode *>(m_initialValue->get()) : std::nullopt;
    }

    Token VariableDeclaration::type() const {
        return m_typeToken;
    }

    bool VariableDeclaration::constant() const {
        return m_constant;
    }
} // ast
