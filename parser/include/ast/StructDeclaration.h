#pragma once
#include <vector>

#include "ASTNode.h"
#include "VariableDeclaration.h"

namespace ast {
    struct StructField {
        Token name;
        std::unique_ptr<RawType> type;
    };

    class StructDeclaration final : public ASTNode {
    private:
        std::vector<StructField> m_fields;
        std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > m_methods;
        std::optional<Token> m_genericParam;

    public:
        StructDeclaration(Token name, std::vector<StructField> fields,
                          std::vector<std::unique_ptr<ast::FunctionDefinitionBase> >
                          methods, std::optional<Token> genericParam) : ASTNode(std::move(name)),
                                                                        m_fields(std::move(fields)),
                                                                        m_methods(std::move(methods)),
                                                                        m_genericParam(std::move(genericParam)) {
        }

        ~StructDeclaration() override = default;

        StructDeclaration(StructDeclaration &&) = default;

        StructDeclaration(const StructDeclaration &) = delete;

        StructDeclaration &operator=(StructDeclaration &&) = delete;

        StructDeclaration &operator=(const StructDeclaration &) = delete;

        [[nodiscard]] const std::vector<StructField> &fields() const { return m_fields; }
        std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > &methods() { return m_methods; }

        [[nodiscard]] std::optional<Token> genericParam() const { return m_genericParam; }
    };
}
