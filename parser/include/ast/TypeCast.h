#pragma once
#include "ASTNode.h"

namespace ast {
    class TypeCast : public ASTNode {
        std::unique_ptr<ASTNode> m_value;
        std::unique_ptr<RawType> m_type;

    public:
        TypeCast(const Token &token, std::unique_ptr<RawType> type, std::unique_ptr<ASTNode> value) : ASTNode(token),
            m_value(std::move(value)), m_type(std::move(type)) {
        }

        ~TypeCast() override = default;

        TypeCast(TypeCast &&) = default;

        TypeCast(const TypeCast &) = delete;

        TypeCast &operator=(TypeCast &&) = delete;

        TypeCast &operator=(const TypeCast &) = delete;

        [[nodiscard]] RawType *rawType() const { return m_type.get(); }
        [[nodiscard]] ASTNode *value() const { return m_value.get(); }

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            return m_value->getNodeByToken(token);
        }
    };
}
