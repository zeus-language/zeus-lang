#pragma once
#include "ASTNode.h"

namespace ast {
    class WhileLoop final : public ASTNode {
        std::vector<std::unique_ptr<ASTNode> > m_block;
        std::unique_ptr<ASTNode> m_condition;

    public:
        WhileLoop(Token whileToken, std::unique_ptr<ASTNode> condition,
                  std::vector<std::unique_ptr<ASTNode> > block) : ASTNode(std::move(whileToken)),
                                                                  m_condition(std::move(condition)),
                                                                  m_block(std::move(block)) {
        }

        ~WhileLoop() override = default;

        [[nodiscard]] ASTNode *condition() const { return m_condition.get(); }

        [[nodiscard]] const std::vector<std::unique_ptr<ASTNode> > &block() const { return m_block; }

        WhileLoop(WhileLoop &&) = default;

        WhileLoop(const WhileLoop &) = delete;

        WhileLoop &operator=(WhileLoop &&) = delete;

        WhileLoop &operator=(const WhileLoop &) = delete;

        std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_condition->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            for (auto &stmt: m_block) {
                result = stmt->getNodeByToken(token);
                if (result.has_value()) {
                    return result;
                }
            }
            return std::nullopt;
        }
    };
}
