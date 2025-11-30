#pragma once
#include <vector>

#include "ASTNode.h"
#include "VariableDeclaration.h"

namespace ast {
    struct StructInitField {
        Token name;
        std::unique_ptr<ASTNode> value;
    };

    class StructInitialization final : public ASTNode {
    private:
        std::optional<Token> m_genericParam;
        std::vector<StructInitField> m_fields;

    public:
        StructInitialization(Token name, std::optional<Token> genericParam,
                             std::vector<StructInitField> fields) : ASTNode(std::move(name)),
                                                                    m_genericParam(std::move(genericParam)),
                                                                    m_fields(std::move(fields)) {
        }

        ~StructInitialization() override = default;

        StructInitialization(StructInitialization &&) = default;

        StructInitialization(const StructInitialization &) = delete;

        StructInitialization &operator=(StructInitialization &&) = delete;

        StructInitialization &operator=(const StructInitialization &) = delete;

        [[nodiscard]] const std::vector<StructInitField> &fields() const { return m_fields; }

        std::string structName() const {
            return expressionToken().lexical() + (m_genericParam.has_value()
                                                      ? "<" + m_genericParam.value().lexical() + ">"
                                                      : "");
        }

        [[nodiscard]] std::optional<Token> genericParam() const {
            return m_genericParam;
        }
    };
}
