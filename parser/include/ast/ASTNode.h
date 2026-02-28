//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_ASTNODE_H
#define ZEUS_LANG_ASTNODE_H
#include <optional>

#include "lexer/Lexer.h"
#include "types/VariableType.h"
namespace ast {
    class ASTNode {
    private:
        Token m_token;
        std::optional<std::shared_ptr<types::VariableType> > m_expressionType = std::nullopt;

    protected:
        explicit ASTNode(Token token) : m_token(std::move(token)) {
        }

    public:
        virtual ~ASTNode() = default;

        virtual std::unique_ptr<ASTNode> clone() {
            assert(false && "clone not implemented for this ASTNode type");
            return nullptr;
        }

        virtual void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) {
            if (!m_expressionType.has_value()) {
                return;
            }
            if (m_expressionType.value()->typeKind() == types::TypeKind::GENERIC) {
                m_expressionType = std::make_optional<std::shared_ptr<types::VariableType> >(genericParam);
            } else if (m_expressionType.value()->typeKind() == types::TypeKind::POINTER ||
                       m_expressionType.value()->typeKind() == types::TypeKind::ARRAY) {
                if (auto ptrType = std::dynamic_pointer_cast<types::PointerType>(m_expressionType.value())) {
                    if (ptrType->baseType()->typeKind() == types::TypeKind::GENERIC) {
                        auto newBaseType = genericParam;
                        auto newPtrType = std::make_shared<types::PointerType>("*" + newBaseType->name(), newBaseType);
                        m_expressionType = std::make_optional<std::shared_ptr<types::VariableType> >(newPtrType);
                    }
                }
            }
        }


        ASTNode(ASTNode &&) = default;

        ASTNode(const ASTNode &) = delete;

        ASTNode &operator=(ASTNode &&) = delete;

        ASTNode &operator=(const ASTNode &) = delete;

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > expressionType() const {
            return m_expressionType;
        }

        void setExpressionType(std::shared_ptr<types::VariableType> type) {
            m_expressionType = std::make_optional<std::shared_ptr<types::VariableType> >(type);
        }

        [[nodiscard]] virtual bool constant() const {
            return false;
        }


        [[nodiscard]] Token expressionToken() const { return m_token; }

        virtual std::optional<ASTNode *> getNodeByToken(const Token &token) const {
            if (m_token == token) {
                return const_cast<ASTNode *>(this);
            }
            return std::nullopt;
        }
    };
} // ast

#endif //ZEUS_LANG_ASTNODE_H
