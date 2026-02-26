//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionDefinition.h"

namespace ast {
    FunctionDefinition::FunctionDefinition(const Token& functionName, std::vector<FunctionArgument> args,
                                           std::optional<std::unique_ptr<RawType> > returnType,
                                           std::vector<std::unique_ptr<ASTNode> > statements,
                                           std::optional<Token> genericParam,
                                           std::vector<std::unique_ptr<RawAnnotation> > annotations,
                                        const VisibilityModifier visibilityModifier)
        : FunctionDefinitionBase(functionName, std::move(args), std::move(returnType),
                                 std::move(annotations),visibilityModifier),
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

    std::unique_ptr<ast::FunctionDefinitionBase> FunctionDefinition::cloneFunction() {
        return cloneFunction2();
    }

    std::unique_ptr<ast::FunctionDefinition> FunctionDefinition::cloneFunction2()
    {
        auto returnTypeClone = returnType().has_value()
                                 ? std::make_optional<std::unique_ptr<RawType> >(returnType().value()->clone())
                                 : std::nullopt;
        std::vector<std::unique_ptr<ASTNode> > statementsClones;
        for (auto &stmt: m_statements) {
            statementsClones.push_back(stmt->clone());
        }
        std::vector<std::unique_ptr<RawAnnotation> > annotationsClones;
        for (auto &annotation: rawAnnotations()) {
            annotationsClones.push_back(annotation->cloneAnnotation());
        }
        auto cloneNode = std::make_unique<FunctionDefinition>(expressionToken(),
                                                              args(),
                                                              std::move(returnTypeClone),
                                                              std::move(statementsClones),
                                                              genericParam,
                                                              std::move(annotationsClones),
                                                              visibilityModifier());
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        return cloneNode;
    }

    bool FunctionDefinition::isMethod() const{
        return m_parentStruct.has_value();
    }

    std::string FunctionDefinitionBase::functionName() const {
        return m_functionName;
    }

    const std::vector<FunctionArgument> &FunctionDefinitionBase::args() const{
        return m_args;
    }

    std::vector<FunctionArgument> & FunctionDefinitionBase::args() {
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


    std::string FunctionDefinitionBase::functionSignature(bool withNamespace) const {
        std::string signature;
        if (withNamespace) {
            for (auto &ns: m_namespacePrefix) {
                signature += ns.lexical() + "::";
            }
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
