#include "ast/VariableDeclaration.h"

namespace ast {
    VariableDeclaration::VariableDeclaration(Token name, std::optional<std::unique_ptr<RawType> > type, bool constant,
                                             std::optional<std::unique_ptr<ASTNode> > initialValue) : ASTNode(std::move(
                name), ast::NodeType::VARIABLE_DECLARATION), m_type(std::move(type)),
        m_constant(constant), m_initialValue(std::move(initialValue)) {
    }

    std::optional<ASTNode *> VariableDeclaration::initialValue() const {
        return m_initialValue.has_value() ? std::make_optional<ASTNode *>(m_initialValue->get()) : std::nullopt;
    }

    std::optional<RawType *> VariableDeclaration::type() const {
        return m_type.has_value() ? std::make_optional<RawType *>(m_type->get()) : std::nullopt;
    }

    bool VariableDeclaration::constant() const {
        return m_constant;
    }

    std::unique_ptr<ASTNode> VariableDeclaration::clone() {
        auto type = m_type.has_value()
                        ? std::make_optional<std::unique_ptr<RawType> >(m_type.value()->clone())
                        : std::nullopt;
        auto cloneNode = std::make_unique<VariableDeclaration>(expressionToken(),
                                                               std::move(type),
                                                               m_constant,
                                                               m_initialValue.has_value()
                                                                   ? std::make_optional<std::unique_ptr<ASTNode> >(
                                                                       m_initialValue.value()->clone())
                                                                   : std::nullopt);
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return std::move(cloneNode);
    }
} // ast
