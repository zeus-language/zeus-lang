
#include "ast/DerefNode.h"

ast::DerefNode::DerefNode(const Token &token,
                          std::unique_ptr<ASTNode> accessNode) : ASTNode(token, NodeType::DEREFERENCE),
                                                                 m_accessNode(std::move(accessNode)) {
}

ast::DerefNode::DerefNode(const DerefNode &other) : ASTNode(other.expressionToken(), NodeType::DEREFERENCE),
                                                    m_accessNode(other.m_accessNode->clone()) {
    if (const auto &type = other.expressionType()) {
        setExpressionType(type.value());
    }
}

ast::ASTNode *ast::DerefNode::accessNode() const {
    return m_accessNode.get();
}

std::unique_ptr<ast::ASTNode> ast::DerefNode::clone() {
    return std::make_unique<DerefNode>(*this);
}
