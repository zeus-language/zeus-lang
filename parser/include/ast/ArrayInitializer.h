#pragma once
#include "ASTNode.h"

namespace ast {
    class ArrayInitializer final : public ASTNode {
    private:
        std::vector<std::unique_ptr<ast::ASTNode> > m_elements;

    public:
        ArrayInitializer(Token token, std::vector<std::unique_ptr<ast::ASTNode> > elements)
            : ASTNode(std::move(token)), m_elements(std::move(elements)) {
        }

        ~ArrayInitializer() override = default;

        ArrayInitializer(ArrayInitializer &&) = default;

        ArrayInitializer(const ArrayInitializer &) = delete;

        ArrayInitializer &operator=(ArrayInitializer &&) = delete;

        ArrayInitializer &operator=(const ArrayInitializer &) = delete;

        [[nodiscard]] const std::vector<std::unique_ptr<ast::ASTNode> > &elements() const { return m_elements; }
    };
}
