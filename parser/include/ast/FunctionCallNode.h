//
// Created by stefan on 29.08.25.
//

#ifndef ZEUS_LANG_FUNCTIONCALLNODE_H
#define ZEUS_LANG_FUNCTIONCALLNODE_H
#include <memory>
#include <vector>

#include "ASTNode.h"
#include "lexer/Lexer.h"

namespace ast {
    class FunctionCallNode : public ASTNode {
        std::vector<Token> m_namespacePrefix;
        std::vector<std::unique_ptr<ASTNode> > m_args;

    public:
        explicit FunctionCallNode(Token functionName, std::vector<Token> namespacePrefix,
                                  std::vector<std::unique_ptr<ASTNode> > args);

        ~FunctionCallNode() override = default;

        std::string functionName() const { return expressionToken().lexical(); }
        std::vector<std::unique_ptr<ASTNode> > &args() { return m_args; }

        [[nodiscard]] std::string functionSignature() const;


        FunctionCallNode(FunctionCallNode &&) = default;

        FunctionCallNode(const FunctionCallNode &) = delete;

        FunctionCallNode &operator=(FunctionCallNode &&) = delete;

        FunctionCallNode &operator=(const FunctionCallNode &) = delete;
    };
} // ast

#endif //ZEUS_LANG_FUNCTIONCALLNODE_H
