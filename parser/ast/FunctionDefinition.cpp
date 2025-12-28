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

    std::shared_ptr<types::VariableType> FunctionDefinitionBase::asFunctionType() const {
        std::vector<std::shared_ptr<types::VariableType> > argTypes;
        std::string name = "fn(";
        for (const auto &arg: m_args) {
            if (arg.type.has_value()) {
                argTypes.push_back(arg.type.value());
            } else {
                argTypes.push_back(nullptr);
            }
            name += argTypes.back() ? argTypes.back()->name() : "unknown";
            name += ", ";
        }
        if (!m_args.empty()) {
            name = name.substr(0, name.size() - 2); // Remove last ", "
        }
        name += ")";

        std::shared_ptr<types::VariableType> returnType = nullptr;
        if (expressionType()) {
            returnType = expressionType().value();
        }
        name += ":" + (returnType ? returnType->name() : "unknown");

        return std::make_shared<types::FunctionType>(name, argTypes, returnType);
    }


    std::string FunctionDefinitionBase::functionSignature() const {
        std::string signature;
        for (auto &ns: m_namespacePrefix) {
            signature += ns.lexical() + "::";
        }

        signature += functionName() + "(";
        for (size_t i = 0; i < m_args.size(); ++i) {
            if (m_args[i].type.has_value()) {
                signature += m_args[i].type.value()->name();
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
