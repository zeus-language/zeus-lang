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

        std::unique_ptr<ASTNode> clone() override {
            auto ifBlock = std::vector<std::unique_ptr<ASTNode> >();
            for (auto &stmt: m_ifBlock) {
                ifBlock.push_back(stmt->clone());
            }
            auto elseBlock = std::vector<std::unique_ptr<ASTNode> >();
            for (auto &stmt: m_elseBlock) {
                elseBlock.push_back(stmt->clone());
            }
            auto conditionClone = m_condition->clone();
            auto cloneNode = std::make_unique<IfCondition>(expressionToken(), std::move(conditionClone),
                                                           std::move(ifBlock), std::move(elseBlock));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return cloneNode;
        }

        void makeNonGeneric(const std::shared_ptr<types::VariableType> &genericParam) override {
            ASTNode::makeNonGeneric(genericParam);
            m_condition->makeNonGeneric(genericParam);
            for (auto &stmt: m_ifBlock) {
                stmt->makeNonGeneric(genericParam);
            }
            for (auto &stmt: m_elseBlock) {
                stmt->makeNonGeneric(genericParam);
            }
        }
    };
}
