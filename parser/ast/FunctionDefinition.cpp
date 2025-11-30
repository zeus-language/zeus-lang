//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionDefinition.h"

namespace ast {
    FunctionDefinition::FunctionDefinition(Token functionName, std::vector<FunctionArgument> args,
                                           std::optional<std::unique_ptr<RawType> > returnType,
                                           std::vector<std::unique_ptr<ASTNode> > statements,
                                           std::optional<Token> genericParam)
        : FunctionDefinitionBase(std::move(functionName), std::move(args), std::move(returnType)),
          m_statements(std::move(statements)), genericParam(std::move(genericParam)) {
    }

    std::optional<ASTNode *> FunctionDefinition::getVariableDefinition(const Token &token) const {
        for (auto &stmt: m_statements) {
            if (const auto node = stmt->getNodeByToken(token)) {
                if (auto varDef = dynamic_cast<VariableDeclaration *>(node.value())) {
                    return varDef;
                }
            }
        }
        return std::nullopt;
    }

    std::string FunctionDefinitionBase::functionName() const {
        return m_functionName;
    }

    std::vector<FunctionArgument> &FunctionDefinitionBase::args() {
        return m_args;
    }


    std::string FunctionDefinitionBase::functionSignature() const {
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
