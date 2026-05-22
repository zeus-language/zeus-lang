#pragma once
#include "ASTNode.h"

namespace ast {
    class IfMacro : public ASTNode {
        std::shared_ptr<ASTNode> m_condition;
        std::vector<std::shared_ptr<ASTNode> > m_ifBlock;
        std::vector<std::shared_ptr<ASTNode> > m_elseBlock;

    public:
        IfMacro(const Token &token, std::shared_ptr<ASTNode> condition, std::vector<std::shared_ptr<ASTNode> > ifBlock,
                std::vector<std::shared_ptr<ASTNode> > elseBlock) : ASTNode(token, NodeType::IF_MACRO),
                                                                    m_condition(std::move(condition)),
                                                                    m_ifBlock(std::move(ifBlock)),
                                                                    m_elseBlock(std::move(elseBlock)) {
        }

        ~IfMacro() override = default;

        IfMacro(IfMacro &&) = default;

        IfMacro(const IfMacro &) = delete;

        IfMacro &operator=(IfMacro &&) = delete;

        IfMacro &operator=(const IfMacro &) = delete;

        [[nodiscard]] ASTNode *condition() const { return m_condition.get(); }

        [[nodiscard]] std::vector<std::shared_ptr<ASTNode> > &ifBlock() { return m_ifBlock; }

        [[nodiscard]] std::vector<std::shared_ptr<ASTNode> > &elseBlock() { return m_elseBlock; }

        std::shared_ptr<ASTNode> clone() override {
            std::vector<std::shared_ptr<ASTNode> > ifBlockClone;
            ifBlockClone.reserve(m_ifBlock.size());
            for (const auto &statement: m_ifBlock) {
                ifBlockClone.push_back(statement->clone());
            }
            std::vector<std::shared_ptr<ASTNode> > elseBlockClone;
            elseBlockClone.reserve(m_elseBlock.size());
            for (const auto &statement: m_elseBlock) {
                elseBlockClone.push_back(statement->clone());
            }
            auto cloneNode = std::make_shared<IfMacro>(expressionToken(), m_condition->clone(), std::move(ifBlockClone),
                                                       std::move(elseBlockClone));
            if (expressionType())
                cloneNode->setExpressionType(expressionType().value());
            return std::move(cloneNode);
        }
    };
}
