//
// Created by stefan on 29.08.25.
//

#include "ast/FunctionDefinition.h"

#include "ast/ReferenceAccess.h"

namespace ast {
    FunctionDefinition::FunctionDefinition(const Token &functionName, std::vector<FunctionArgument> args,
                                           std::optional<std::unique_ptr<RawType> > returnType,
                                           std::unique_ptr<BlockNode> blockNode,
                                           std::optional<Token> genericParam,
                                           std::vector<std::unique_ptr<RawAnnotation> > annotations,
                                           const VisibilityModifier visibilityModifier)
        : FunctionDefinitionBase(functionName, std::move(args), std::move(returnType),
                                 std::move(annotations), visibilityModifier),
          m_blockNode(std::move(blockNode)), genericParam(std::move(genericParam)) {
    }

    std::optional<ASTNode *> FunctionDefinition::getVariableDefinition(const Token &token) const {
        if (auto accessNode = m_blockNode->getVariableDefinition(token.lexical()); accessNode.has_value()) {
            return accessNode;
        }

        for (const auto &arg: args()) {
            if (arg.name == token) {
                return std::make_optional<ASTNode *>(const_cast<FunctionDefinition *>(this));
            }
        }
        return std::nullopt;
    }

    std::unique_ptr<ast::FunctionDefinitionBase> FunctionDefinition::cloneFunction() {
        return cloneFunction2();
    }

    std::unique_ptr<ast::FunctionDefinition> FunctionDefinition::cloneFunction2() {
        auto returnTypeClone = returnType().has_value()
                                   ? std::make_optional<std::unique_ptr<RawType> >(returnType().value()->clone())
                                   : std::nullopt;
        auto block = m_blockNode->cloneBlock();
        std::vector<std::unique_ptr<RawAnnotation> > annotationsClones;
        for (const auto &annotation: rawAnnotations()) {
            annotationsClones.push_back(annotation->cloneAnnotation());
        }
        auto cloneNode = std::make_unique<FunctionDefinition>(expressionToken(),
                                                              args(),
                                                              std::move(returnTypeClone),
                                                              std::move(block),
                                                              genericParam,
                                                              std::move(annotationsClones),
                                                              visibilityModifier());
        if (expressionType())
            cloneNode->setExpressionType(expressionType().value());
        cloneNode->setModulePath(modulePath());
        return std::move(cloneNode);
    }

    bool FunctionDefinition::isMethod() const {
        return m_parentStruct.has_value();
    }

    std::optional<ASTNode *> FunctionDefinition::getNodeByToken(const Token &token) const {
        if (auto result = FunctionDefinitionBase::getNodeByToken(token)) {
            return result;
        }
        for (const auto &arg: args()) {
            if (arg.name == token) {
                return std::make_optional<ASTNode *>(const_cast<FunctionDefinition *>(this));
            }
        }
        if (auto result = block()->getNodeByToken(token)) {
            return result;
        }
        return std::nullopt;
    }

    void FunctionDefinitionBase::setModulePath(const std::vector<Token> &module_path) {
        m_namespacePrefix = module_path;

        m_modulePathName = "";
        for (auto &ns: m_namespacePrefix) {
            m_modulePathName += ns.lexical() + "::";
        }
    }

    const std::string &FunctionDefinitionBase::modulePathName() const {
        return m_modulePathName;
    }

    const std::string &FunctionDefinitionBase::functionName() const {
        return m_functionName;
    }

    const std::vector<FunctionArgument> &FunctionDefinitionBase::args() const {
        return m_args;
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


    std::string FunctionDefinitionBase::functionSignature(const bool withNamespace) const {
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
