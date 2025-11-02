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
    };
}
