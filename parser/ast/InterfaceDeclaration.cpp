//
// Created by stefan on 26.05.26.
//

#include "ast/InterfaceDeclaration.h"

namespace ast {
    InterfaceDeclaration::InterfaceDeclaration(const Token &nameToken,
                                               std::vector<std::shared_ptr<FunctionSignature> > functions)
        : ASTNode(nameToken, NodeType::INTERFACE_DECLARATION), m_methods(std::move(functions)) {
    }

    std::shared_ptr<ast::ASTNode> InterfaceDeclaration::clone() {
        std::vector<std::shared_ptr<ast::FunctionSignature> > methodsClone;
        methodsClone.reserve(m_methods.size());
        for (const auto &method: m_methods) {
            methodsClone.push_back(method->cloneSignature());
        }
        auto cloneNode = std::make_shared<InterfaceDeclaration>(expressionToken(),
                                                                std::move(methodsClone));
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return std::move(cloneNode);
    }

    std::optional<ASTNode *> InterfaceDeclaration::getNodeByToken(const Token &token) const {
        if (expressionToken() == token) {
            return const_cast<InterfaceDeclaration *>(this);
        }

        for (const auto &method: m_methods) {
            if (const auto result = method->getNodeByToken(token)) {
                return result;
            }
        }
        return std::nullopt;
    }
}
