#pragma once
#include <memory>
#include <utility>
#include <vector>

#include "ASTNode.h"

namespace ast {
    class InterpolatedString final : public ast::ASTNode {
    private:
        std::vector<std::shared_ptr<ast::ASTNode> > m_nodes;
        std::shared_ptr<ast::ASTNode> m_accessNode;

    public:
        explicit InterpolatedString(Token token, std::shared_ptr<ast::ASTNode> accessNode,
                                    std::vector<std::shared_ptr<ast::ASTNode> > nodes) : ASTNode(std::move(token)),
            m_nodes(std::move(nodes)), m_accessNode(std::move(accessNode)) {
        }

        [[nodiscard]] const std::vector<std::shared_ptr<ast::ASTNode> > &nodes() const { return m_nodes; }
        [[nodiscard]] const std::shared_ptr<ast::ASTNode> &accessNode() const { return m_accessNode; }
    };
}
