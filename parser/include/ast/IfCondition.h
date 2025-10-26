#pragma once
#include <memory>

#include "ASTNode.h"

namespace ast {
    class IfCondition final : public ASTNode {
        std::unique_ptr<ASTNode> m_condition;
        std::vector<std::unique_ptr<ASTNode> > m_ifBlock;
        std::vector<std::unique_ptr<ASTNode> > m_elseBlock;

    public:
        IfCondition(Token token, std::unique_ptr<ASTNode> condition, std::vector<std::unique_ptr<ASTNode> > ifBlock,
                    std::vector<std::unique_ptr<ASTNode> > elseBlock) : ASTNode(std::move(token)),
                                                                        m_condition(std::move(condition)),
                                                                        m_ifBlock(std::move(ifBlock)),
                                                                        m_elseBlock(std::move(elseBlock)) {
        }

        ~IfCondition() override = default;

        [[nodiscard]] ASTNode *condition() const { return m_condition.get(); }

        [[nodiscard]] std::vector<std::unique_ptr<ASTNode> > &ifBlock() { return m_ifBlock; }

        [[nodiscard]] std::vector<std::unique_ptr<ASTNode> > &elseBlock() { return m_elseBlock; }

        IfCondition(IfCondition &&) = default;

        IfCondition(const IfCondition &) = delete;

        IfCondition &operator=(IfCondition &&) = delete;

        IfCondition &operator=(const IfCondition &) = delete;

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_condition->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            for (auto &stmt: m_ifBlock) {
                result = stmt->getNodeByToken(token);
                if (result.has_value()) {
                    return result;
                }
            }
            for (auto &stmt: m_elseBlock) {
                result = stmt->getNodeByToken(token);
                if (result.has_value()) {
                    return result;
                }
            }
            return std::nullopt;
        }
    };
}
