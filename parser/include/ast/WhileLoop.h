#pragma once
#include "ASTNode.h"

namespace ast {
    class WhileLoop final : public ASTNode {
        std::shared_ptr<BlockNode> m_block;
        std::shared_ptr<ASTNode> m_condition;

    public:
        WhileLoop(Token whileToken, std::shared_ptr<ASTNode> condition,
                  std::shared_ptr<BlockNode> block) : ASTNode(std::move(whileToken), NodeType::WHILE_LOOP),
                                                      m_block(std::move(block)),
                                                      m_condition(std::move(condition)) {
        }

        ~WhileLoop() override = default;

        [[nodiscard]] ASTNode *condition() const { return m_condition.get(); }

        [[nodiscard]] BlockNode *block() const { return m_block.get(); }

        WhileLoop(WhileLoop &&) = default;

        WhileLoop(const WhileLoop &) = delete;

        WhileLoop &operator=(WhileLoop &&) = delete;

        WhileLoop &operator=(const WhileLoop &) = delete;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override {
            auto result = m_condition->getNodeByToken(token);
            if (result.has_value()) {
                return result;
            }
            return m_block->getNodeByToken(token);
        }

        std::shared_ptr<ASTNode> clone() override {
            auto blockClones = m_block->cloneBlock();
            auto cloneNode = std::make_shared<WhileLoop>(expressionToken(),
                                                         m_condition->clone(),
                                                         std::move(blockClones));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }
    };
}
