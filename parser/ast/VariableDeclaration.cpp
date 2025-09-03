#include "ast/VariableDeclaration.h"

namespace ast {
    VariableDeclaration::VariableDeclaration(Token name, Token type, bool constant,
                                             std::optional<std::unique_ptr<ASTNode> > initialValue) : ASTNode(std::move(
            name)), m_type(std::move(type)),
        m_initialValue(std::move(initialValue)), m_constant(constant) {
    }

    std::optional<std::unique_ptr<ASTNode> > VariableDeclaration::initialValue() {
        return std::move(m_initialValue);
    }

    Token VariableDeclaration::type() const {
        return m_type;
    }

    bool VariableDeclaration::constant() const {
        return m_constant;
    }
} // ast
