//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionCallNode.h"

namespace ast {
    FunctionCallNode::FunctionCallNode(Token functionName, std::vector<Token> namespacePrefix,
                                       std::vector<std::unique_ptr<ASTNode> > args, std::optional<Token> genericParam)
        : ASTNode(std::move(functionName)), m_namespacePrefix(std::move(namespacePrefix)), m_args(std::move(args)),
          m_genericParam(std::move(genericParam)) {
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

    std::unique_ptr<ASTNode> FunctionCallNode::clone() {
        std::vector<std::unique_ptr<ASTNode> > argClones;
        for (auto &arg: m_args) {
            argClones.push_back(arg->clone());
        }
        auto cloneNode = std::make_unique<FunctionCallNode>(expressionToken(),
                                                            m_namespacePrefix,
                                                            std::move(argClones),
                                                            m_genericParam);
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        if (genericType()) {
            cloneNode->setGenericType(genericType().value());
        }
        return cloneNode;
    }
} // ast
