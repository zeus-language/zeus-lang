#pragma once
#include "ASTNode.h"

namespace ast {
    class EnumValue final : public ASTNode {
    private:
        Token m_variantName;
        ASTNode *m_accessNode = nullptr;

    public:
        explicit EnumValue(Token enumName, Token variantName) : ASTNode(std::move(enumName)),
                                                                m_variantName(std::move(variantName)) {
        }

        ~EnumValue() override = default;

        EnumValue(EnumValue &&) = default;

        EnumValue(const EnumValue &) = delete;

        EnumValue &operator=(EnumValue &&) = delete;

        EnumValue &operator=(const EnumValue &) = delete;

        [[nodiscard]] Token variantName() const {
            return m_variantName;
        }

        void setAccessNode(ast::ASTNode *accessNode) {
            m_accessNode = accessNode;
        }

        [[nodiscard]] ast::ASTNode *accessNode() const {
            return m_accessNode;
        }

        std::shared_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_shared<EnumValue>(expressionToken(), m_variantName);
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }
    };
}
