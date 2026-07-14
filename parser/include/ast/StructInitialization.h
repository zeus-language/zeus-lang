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
        std::optional<Token> m_genericParam;
        std::vector<StructInitField> m_fields;
        std::optional<Token> m_unionName;

    public:
        StructInitialization(Token name, std::optional<Token> genericParam,
                             std::vector<StructInitField> fields,
                             std::optional<Token> unionName) : ASTNode(std::move(name)),
                                                               m_genericParam(std::move(genericParam)),
                                                               m_fields(std::move(fields)),
                                                               m_unionName(std::move(unionName)) {
        }

        ~StructInitialization() override = default;

        StructInitialization(StructInitialization &&) = default;

        StructInitialization(const StructInitialization &) = delete;

        StructInitialization &operator=(StructInitialization &&) = delete;

        StructInitialization &operator=(const StructInitialization &) = delete;

        [[nodiscard]] const std::vector<StructInitField> &fields() const { return m_fields; }

        [[nodiscard]] std::string structName() const {
            return expressionToken().lexical() + (m_genericParam.has_value()
                                                      ? "<" + m_genericParam.value().lexical() + ">"
                                                      : "");
        }

        [[nodiscard]] const std::optional<Token> &genericParam() const {
            return m_genericParam;
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
                                                                    m_genericParam,
                                                                    std::move(fieldsClone), unionName);
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }
    };
}
