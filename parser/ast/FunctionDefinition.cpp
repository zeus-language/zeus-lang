//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionDefinition.h"

namespace ast {
    FunctionDefinition::FunctionDefinition(Token functionName, std::vector<FunctionArgument> args,
                                           std::optional<std::unique_ptr<RawType> > returnType,
                                           std::vector<std::unique_ptr<ASTNode> > statements)
        : ASTNode(std::move(functionName)), m_args(std::move(args)), m_returnType(std::move(returnType)),
          m_statements(std::move(statements)) {
    }

    std::string FunctionDefinition::functionName() const {
        return expressionToken().lexical();
    }

    std::vector<FunctionArgument> &FunctionDefinition::args() {
        return m_args;
    }

    std::string FunctionDefinition::functionSignature() const {
        std::string signature;
        for (auto &ns: m_namespacePrefix) {
            signature += ns.lexical() + "::";
        }

        signature += functionName() + "(";
        for (size_t i = 0; i < m_args.size(); ++i) {
            signature += m_args[i].rawType->typeToken.lexical();

            if (i < m_args.size() - 1) {
                signature += ", ";
            }
        }
        signature += ")";
        return signature;
    }
} // ast
