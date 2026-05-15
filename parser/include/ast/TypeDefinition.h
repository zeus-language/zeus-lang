#pragma once
#include "ASTNode.h"
#include "VariableDeclaration.h"

namespace ast {
    class TypeDefinition : public ASTNode {
        std::shared_ptr<ast::RawType> m_type;

    public:
        TypeDefinition(const Token &token,
                       std::shared_ptr<ast::RawType> type) : ASTNode(token), m_type(std::move(type)) {
        }

        const std::shared_ptr<ast::RawType> &getType() const {
            return m_type;
        }

        std::shared_ptr<ASTNode> clone() override {
            auto cloned = std::make_shared<TypeDefinition>(expressionToken(), m_type->clone());
            if (expressionType()) {
                cloned->setExpressionType(expressionType().value());
            }
            return std::move(cloned);
        }
    };
}
