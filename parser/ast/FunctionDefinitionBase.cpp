#include "ast/FunctionDefinitionBase.h"

namespace ast {
    FunctionDefinitionBase::FunctionDefinitionBase(const Token &functionName, std::vector<FunctionArgument> args,
                                                   std::optional<std::shared_ptr<RawType> > returnType,
                                                   std::vector<std::shared_ptr<RawAnnotation> > annotations,
                                                   const VisibilityModifier visibilityModifier) : AnnotatedNode(
            functionName, ast::NodeType::FUNCTION_DEFINITION),
        m_functionName(expressionToken().lexical()),
        m_args(std::move(args)),
        m_returnType(std::move(returnType)),
        m_rawAnnotations(std::move(annotations)),
        m_visibilityModifier(visibilityModifier) {
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
            name += ",";
        }
        if (!m_args.empty()) {
            name = name.substr(0, name.size() - 1); // Remove last ","
        }
        name += ")";

        std::shared_ptr<types::VariableType> returnType = nullptr;
        if (resolvedReturnType()) {
            returnType = resolvedReturnType().value();
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
}
