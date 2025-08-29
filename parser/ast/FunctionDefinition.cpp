//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionDefinition.h"

namespace ast {
    FunctionDefinition::FunctionDefinition(Token functionName, std::vector<FunctionArgument> args,
                                           std::vector<std::unique_ptr<ASTNode> > statements)
        : ASTNode(std::move(functionName)), m_args(std::move(args)), m_statements(std::move(statements)) {
    }

    std::string FunctionDefinition::functionName() const {
        return expressionToken().lexical();
    }

    std::vector<FunctionArgument> &FunctionDefinition::args() {
        return m_args;
    }
} // ast
