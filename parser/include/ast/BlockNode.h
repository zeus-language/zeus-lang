#pragma once
#include "ASTNode.h"

namespace ast {
    class BlockNode : public ASTNode {
    private:
        std::vector<std::unique_ptr<ASTNode> > m_statements;

    public:
        BlockNode(const Token &token, std::vector<std::unique_ptr<ASTNode> > statements);

        [[nodiscard]] const std::vector<std::unique_ptr<ASTNode> > &statements();

        [[nodiscard]] std::unique_ptr<BlockNode> cloneBlock() const;

        std::unique_ptr<ASTNode> clone() override;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override;

        [[nodiscard]] std::optional<ASTNode *> getVariableDefinition(const std::string &name) const;
    };
} // namespace ast
