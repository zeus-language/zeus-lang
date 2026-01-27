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
        std::unique_ptr<ASTNode> clone() override;
    };

    inline std::unique_ptr<ASTNode> StructDeclaration::clone() {
        std::vector<StructField> fieldsClone;
        for (const auto &field: m_fields) {
            fieldsClone.push_back(StructField{
                .name = field.name,
                .type = field.type->clone()
            });
        }
        std::vector<std::unique_ptr<ast::FunctionDefinitionBase> > methodsClone;
        for (const auto &method: m_methods) {
            methodsClone.push_back(method->cloneFunction());
        }
        auto cloneNode = std::make_unique<StructDeclaration>(expressionToken(),
                                                            std::move(fieldsClone),
                                                            std::move(methodsClone),
                                                            m_genericParam);
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return cloneNode;
    }
}
