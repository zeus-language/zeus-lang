#pragma once

#include "ast/ASTNode.h"
#include "ast/FunctionSignature.h"

namespace ast {
    class InterfaceDeclaration final : public ASTNode {
    private:
        std::vector<std::shared_ptr<FunctionSignature> > m_methods;

    public:
        InterfaceDeclaration(const Token &nameToken, std::vector<std::shared_ptr<FunctionSignature> > functions);


        const std::vector<std::shared_ptr<FunctionSignature> > &methods() const { return m_methods; }

        InterfaceDeclaration(InterfaceDeclaration &&) = default;

        InterfaceDeclaration(const InterfaceDeclaration &) = delete;

        InterfaceDeclaration &operator=(InterfaceDeclaration &&) = delete;

        InterfaceDeclaration &operator=(const InterfaceDeclaration &) = delete;

        std::shared_ptr<ast::ASTNode> clone() override;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override;
    };
} // namespace ast

