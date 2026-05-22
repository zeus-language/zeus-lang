#pragma once
#include "ASTNode.h"

namespace ast {
    class ArrayInitializer final : public ASTNode {
    private:
        std::vector<std::shared_ptr<ast::ASTNode> > m_elements;

    public:
        ArrayInitializer(Token token, std::vector<std::shared_ptr<ast::ASTNode> > elements)
            : ASTNode(std::move(token)), m_elements(std::move(elements)) {
        }

        ~ArrayInitializer() override = default;

        ArrayInitializer(ArrayInitializer &&) = default;

        ArrayInitializer(const ArrayInitializer &) = delete;

        ArrayInitializer &operator=(ArrayInitializer &&) = delete;

        ArrayInitializer &operator=(const ArrayInitializer &) = delete;

        [[nodiscard]] const std::vector<std::shared_ptr<ast::ASTNode> > &elements() const { return m_elements; }

        [[nodiscard]] bool constant() const override {
            for (const auto &element: m_elements) {
                if (!element->constant()) {
                    return false;
                }
            }
            return true;
        }

        std::shared_ptr<ASTNode> clone() override {
            std::vector<std::shared_ptr<ast::ASTNode> > elementClones;
            for (const auto &element: m_elements) {
                elementClones.push_back(element->clone());
            }
            auto cloneNode = std::make_shared<ArrayInitializer>(expressionToken(),
                                                                std::move(elementClones));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }
    };
}
