#pragma once
#include "ASTNode.h"
#include "VariableDeclaration.h"

namespace ast {
    enum class EnumVariantType {
        UNIT,
        TUPLE,
        STRUCT,
    };

    struct EnumVariant {
        Token name;
        EnumVariantType type = EnumVariantType::UNIT;
        std::optional<std::unique_ptr<ast::ASTNode> > value;
        std::vector<std::unique_ptr<RawType> > associatedRawTypes; // for TUPLE
        std::vector<VariableDeclaration> fields; // for STRUCT
    };

    class EnumDeclaration final : public ASTNode {
    private:
        std::vector<EnumVariant> m_variants;

    public:
        explicit EnumDeclaration(Token name, std::vector<EnumVariant> variants) : ASTNode(std::move(name)),
            m_variants(std::move(variants)) {
        }

        ~EnumDeclaration() override = default;

        EnumDeclaration(EnumDeclaration &&) = default;

        EnumDeclaration(const EnumDeclaration &) = delete;

        EnumDeclaration &operator=(EnumDeclaration &&) = delete;

        EnumDeclaration &operator=(const EnumDeclaration &) = delete;

        [[nodiscard]] const std::vector<EnumVariant> &variants() const { return m_variants; }
    };
}
