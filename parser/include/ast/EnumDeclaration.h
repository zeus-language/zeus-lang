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
        std::optional<std::unique_ptr<ast::ASTNode> > value = std::nullopt;
        std::vector<std::unique_ptr<RawType> > associatedRawTypes; // for TUPLE
        std::vector<VariableDeclaration> fields; // for STRUCT
        EnumVariant(Token name, std::optional<std::unique_ptr<ast::ASTNode> > value) : name(std::move(name)),
            type(EnumVariantType::UNIT), value(std::move(value)) {
        }

        EnumVariant(Token name, EnumVariantType type, std::optional<std::unique_ptr<ast::ASTNode> > value,
                    std::vector<std::unique_ptr<RawType> > associatedRawTypes, std::vector<VariableDeclaration> fields)
            : name(std::move(name)), type(type), value(std::move(value)),
              associatedRawTypes(std::move(associatedRawTypes)), fields(std::move(fields)) {
        }

        EnumVariant(EnumVariant &&) = default;

        EnumVariant(const EnumVariant &other) : name(other.name), type(other.type) {
            this->name = other.name;
            this->type = other.type;
            this->value = other.value
                              ? std::make_optional<std::unique_ptr<ast::ASTNode> >(other.value.value()->clone())
                              : std::nullopt;
            for (const auto &rawType: other.associatedRawTypes) {
                this->associatedRawTypes.push_back(rawType->clone());
            }
            for (const auto &field: other.fields) {
                this->fields.push_back(field.copy());
            }
        }
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

        std::unique_ptr<ASTNode> clone() override {
            std::vector<EnumVariant> variants;
            for (const auto &variant: m_variants) {
                variants.emplace_back(variant);
            }
            auto node = std::make_unique<EnumDeclaration>(expressionToken(), std::move(variants));
            if (expressionType())
                node->setExpressionType(expressionType().value());
            return std::move(node);
        }
    };
}
