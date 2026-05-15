#pragma once
#include "ASTNode.h"

namespace ast {
    class BlockNode : public ASTNode {
    private:
        std::vector<std::shared_ptr<ASTNode> > m_statements;

    public:
        BlockNode(const Token &token, std::vector<std::shared_ptr<ASTNode> > statements);

        [[nodiscard]] std::vector<std::shared_ptr<ASTNode> > &statements();

        void setStatements(std::vector<std::shared_ptr<ASTNode> > statements);

        [[nodiscard]] std::shared_ptr<BlockNode> cloneBlock() const;

        std::shared_ptr<ASTNode> clone() override;

        [[nodiscard]] std::optional<ASTNode *> getNodeByToken(const Token &token) const override;

        [[nodiscard]] std::optional<ASTNode *> getVariableDefinition(const std::string &name) const;
    };
} // namespace ast
