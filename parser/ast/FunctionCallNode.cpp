//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionCallNode.h"

namespace ast {
    FunctionCallNode::FunctionCallNode(Token functionName, std::vector<Token> namespacePrefix,
                                       std::vector<std::unique_ptr<ASTNode> > args)
        : ASTNode(std::move(functionName)), m_namespacePrefix(std::move(namespacePrefix)), m_args(std::move(args)) {
    }

    std::string FunctionCallNode::functionSignature() const {
        std::string signature;
        for (auto &ns: m_namespacePrefix) {
            signature += ns.lexical() + "::";
        }

        signature += functionName() + "(";
        for (size_t i = 0; i < m_args.size(); ++i) {
            if (m_args[i]->expressionType().has_value()) {
                signature += m_args[i]->expressionType().value()->name();
            } else {
                signature += "unknown";
            }
            if (i < m_args.size() - 1) {
                signature += ", ";
            }
        }
        signature += ")";
        return signature;
    }
} // ast
