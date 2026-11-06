#pragma once
#include <vector>

#include "ASTNode.h"
#include "VariableDeclaration.h"

namespace ast {
    struct StructInitField {
        Token name;
        std::shared_ptr<ASTNode> value;
    };

    class StructInitialization final : public ASTNode {
    private:
        std::vector<StructInitField> m_fields;
        std::optional<Token> m_unionName;
        std::shared_ptr<RawType> m_type;

    public:
        StructInitialization(Token name, const std::shared_ptr<RawType> &type,
                             std::vector<StructInitField> fields,
                             std::optional<Token> unionName) : ASTNode(std::move(name)),
                                                               m_fields(std::move(fields)),
                                                               m_unionName(std::move(unionName)),
                                                               m_type(type) {
        }

        ~StructInitialization() override = default;

        StructInitialization(StructInitialization &&) = default;

        StructInitialization(const StructInitialization &) = delete;

        StructInitialization &operator=(StructInitialization &&) = delete;

        StructInitialization &operator=(const StructInitialization &) = delete;

        [[nodiscard]] const std::vector<StructInitField> &fields() const { return m_fields; }


        [[nodiscard]] std::string structName() const {
            return m_type->fullTypeName();
        }

        [[nodiscard]] std::shared_ptr<RawType> &rawType() {
            return m_type;
        }


        [[nodiscard]] std::optional<Token> &unionName() {
            return m_unionName;
        }

        std::shared_ptr<ASTNode> clone() override {
            std::vector<StructInitField> fieldsClone;
            fieldsClone.reserve(m_fields.size());
            for (const auto &field: m_fields) {
                fieldsClone.push_back(StructInitField{
                    .name = field.name,
                    .value = field.value->clone(),
                });
            }
            std::optional<Token> unionName = m_unionName;

            auto cloneNode = std::make_shared<StructInitialization>(expressionToken(),
                                                                    m_type->clone(),
                                                                    std::move(fieldsClone), unionName);
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }
    };
}
