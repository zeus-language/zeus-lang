#pragma once
#include "ASTNode.h"

namespace ast {
    class EnumAccess final : public ASTNode {
    private:
        Token m_variantName;

    public:
        explicit EnumAccess(Token enumName, Token variantName) : ASTNode(std::move(enumName)),
                                                                 m_variantName(std::move(variantName)) {
        }

        ~EnumAccess() override = default;

        EnumAccess(EnumAccess &&) = default;

        EnumAccess(const EnumAccess &) = delete;

        EnumAccess &operator=(EnumAccess &&) = delete;

        EnumAccess &operator=(const EnumAccess &) = delete;

        [[nodiscard]] Token variantName() const {
            return m_variantName;
        }
    };
}
