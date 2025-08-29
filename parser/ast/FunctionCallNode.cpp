//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionCallNode.h"

namespace ast {
    FunctionCallNode::FunctionCallNode(Token functionName, std::vector<std::unique_ptr<ASTNode> > args)
        : ASTNode(std::move(functionName)), m_args(std::move(args)) {
    }
} // ast
