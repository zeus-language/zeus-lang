
#include "ast/DerefNode.h"

ast::DerefNode::DerefNode(const Token &token,
                          std::shared_ptr<ASTNode> accessNode) : ASTNode(token, NodeType::DEREFERENCE),
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

std::shared_ptr<ast::ASTNode> ast::DerefNode::clone() {
    return std::make_shared<DerefNode>(*this);
}
