#pragma once
#include "ASTNode.h"
#include "VariableDeclaration.h"

namespace ast {
    enum class UnionVariantType {
        UNIT,
        TUPLE,
        STRUCT,
    };

    struct UnionVariant {
        Token name;
        UnionVariantType type = UnionVariantType::UNIT;
        std::vector<std::shared_ptr<RawType> > associatedRawTypes; // for TUPLE
        std::vector<StructField> fields; // for STRUCT
        UnionVariant(Token name) : name(std::move(name)) {
        }

        UnionVariant(Token name, const UnionVariantType type,
                     std::vector<std::shared_ptr<RawType> > associatedRawTypes,
                     std::vector<StructField> fields)
            : name(std::move(name)), type(type),
              associatedRawTypes(std::move(associatedRawTypes)), fields(std::move(fields)) {
        }

        UnionVariant(UnionVariant &&) = default;

        UnionVariant(const UnionVariant &other) : name(other.name), type(other.type) {
            this->name = other.name;
            this->type = other.type;
            for (const auto &rawType: other.associatedRawTypes) {
                this->associatedRawTypes.push_back(rawType->clone());
            }
            for (const auto &field: other.fields) {
                this->fields.push_back(field);
            }
        }
    };

    class UnionDeclaration final : public ASTNode {
    private:
        std::vector<UnionVariant> m_variants;
        std::vector<Token> m_genericArguments;

    public:
        explicit UnionDeclaration(Token name, std::vector<Token> genericArguments,
                                  std::vector<UnionVariant> variants) : ASTNode(std::move(name)),
                                                                        m_variants(std::move(variants)),
                                                                        m_genericArguments(
                                                                            std::move(genericArguments)) {
        }

        ~UnionDeclaration() override = default;

        UnionDeclaration(UnionDeclaration &&) = default;

        UnionDeclaration(const UnionDeclaration &) = delete;

        UnionDeclaration &operator=(UnionDeclaration &&) = delete;

        UnionVariantType &operator=(const UnionDeclaration &) = delete;

        [[nodiscard]] const std::vector<UnionVariant> &variants() const { return m_variants; }

        std::shared_ptr<ASTNode> clone() override {
            std::vector<UnionVariant> variants;
            for (const auto &variant: m_variants) {
                variants.emplace_back(variant);
            }
            std::vector<Token> genericArguments(m_genericArguments);
            auto node = std::make_shared<UnionDeclaration>(expressionToken(), std::move(genericArguments),
                                                           std::move(variants));
            if (expressionType())
                node->setExpressionType(expressionType().value());
            return std::move(node);
        }
    };
}
