#pragma once
#include "ASTNode.h"

namespace ast {
    class ArrayRepeatInitializer final : public ASTNode {
    private:
        std::unique_ptr<ast::ASTNode> m_value;
        std::unique_ptr<ast::ASTNode> m_count;

    public:
        ArrayRepeatInitializer(Token token, std::unique_ptr<ast::ASTNode> value,
                               std::unique_ptr<ast::ASTNode> count)
            : ASTNode(std::move(token)), m_value(std::move(value)), m_count(std::move(count)) {
        }

        ~ArrayRepeatInitializer() override = default;

        ArrayRepeatInitializer(ArrayRepeatInitializer &&) = default;

        ArrayRepeatInitializer(const ArrayRepeatInitializer &) = delete;

        ArrayRepeatInitializer &operator=(ArrayRepeatInitializer &&) = delete;

        ArrayRepeatInitializer &operator=(const ArrayRepeatInitializer &) = delete;

        [[nodiscard]] ASTNode *value() const {
            return m_value.get();
        }

        [[nodiscard]] ASTNode *count() const {
            return m_count.get();
        }

        [[nodiscard]] bool constant() const override {
            return true;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto values = m_value->clone();
            auto counts = m_count->clone();
            auto cloneNode = std::make_unique<ArrayRepeatInitializer>(expressionToken(),
                                                                      std::move(values), std::move(counts));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }
    };
}
