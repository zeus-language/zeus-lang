#pragma once
#include "ASTNode.h"

namespace ast {
    class IfMacro : public ASTNode {
        std::unique_ptr<ASTNode> m_condition;
        std::vector<std::unique_ptr<ASTNode> > m_ifBlock;
        std::vector<std::unique_ptr<ASTNode> > m_elseBlock;

    public:
        IfMacro(const Token &token, std::unique_ptr<ASTNode> condition, std::vector<std::unique_ptr<ASTNode> > ifBlock,
                std::vector<std::unique_ptr<ASTNode> > elseBlock) : ASTNode(token, NodeType::IF_MACRO),
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

        [[nodiscard]] std::vector<std::unique_ptr<ASTNode> > &ifBlock() { return m_ifBlock; }

        [[nodiscard]] std::vector<std::unique_ptr<ASTNode> > &elseBlock() { return m_elseBlock; }
    };
}
