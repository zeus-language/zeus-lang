#pragma once
#include "ASTNode.h"

namespace ast {
    class RangeExpression final : public ASTNode {
    private:
        std::unique_ptr<ASTNode> m_start;
        std::unique_ptr<ASTNode> m_end;
        bool m_is_inclusive = false;

    public:
        explicit RangeExpression(Token name, std::unique_ptr<ASTNode> start,
                                 std::unique_ptr<ASTNode> end, const bool is_inclusive) : ASTNode(std::move(name)),
            m_start(std::move(start)),
            m_end(std::move(end)), m_is_inclusive(is_inclusive) {
        }

        ~RangeExpression() override = default;

        RangeExpression(RangeExpression &&) = default;

        RangeExpression(const RangeExpression &) = delete;

        RangeExpression &operator=(RangeExpression &&) = delete;

        RangeExpression &operator=(const RangeExpression &) = delete;

        [[nodiscard]] ASTNode *start() const {
            return m_start.get();
        }

        [[nodiscard]] ASTNode *end() const {
            return m_end.get();
        }

        [[nodiscard]] bool constant() const override {
            return m_start->constant() && m_end->constant();
        }

        [[nodiscard]] bool isInclusive() const {
            return m_is_inclusive;
        }

        std::unique_ptr<ASTNode> clone() override {
            auto cloneNode = std::make_unique<RangeExpression>(expressionToken(),
                                                               m_start->clone(),
                                                               m_end->clone(),
                                                               m_is_inclusive);
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_start->makeNonGeneric(genericParam);
            m_end->makeNonGeneric(genericParam);
        }
    };
}
