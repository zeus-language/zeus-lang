#pragma once
#include <memory>
#include <vector>

#include "AnnotatedNode.h"
#include "BlockNode.h"
#include "RawAnnotation.h"
#include "VariableDeclaration.h"
#include "lexer/Lexer.h"

namespace ast {
    struct FunctionArgument {
        Token name;
        std::unique_ptr<RawType> rawType;
        bool isConstant;

        std::optional<std::shared_ptr<types::VariableType> > type = std::nullopt;

        FunctionArgument(Token name, std::unique_ptr<RawType> type, const bool isConstant)
            : name(std::move(name)), rawType(std::move(type)), isConstant(isConstant) {
        }

        FunctionArgument(const FunctionArgument &other)
            : name(other.name), rawType(other.rawType->clone()), isConstant(other.isConstant), type(other.type) {
        }
    };

    class FunctionDefinitionBase : public AnnotatedNode {
        std::string m_functionName;
        std::vector<FunctionArgument> m_args;
        std::optional<std::unique_ptr<RawType> > m_returnType;
        std::optional<std::shared_ptr<types::VariableType> > m_resolvedReturnTyoe = std::nullopt;
        std::vector<Token> m_namespacePrefix;
        std::vector<std::unique_ptr<RawAnnotation> > m_rawAnnotations;
        VisibilityModifier m_visibilityModifier = VisibilityModifier::PRIVATE;
        std::string m_modulePathName;

    protected:
        void setFunctionName(const std::string &functionName) {
            m_functionName = functionName;
        }

    public:
        explicit FunctionDefinitionBase(const Token &functionName, std::vector<FunctionArgument> args,
                                        std::optional<std::unique_ptr<RawType> > returnType,
                                        std::vector<std::unique_ptr<RawAnnotation> > annotations,
                                        const VisibilityModifier visibilityModifier);

        [[nodiscard]] std::optional<RawType *> returnType() const {
            return m_returnType.has_value() ? std::make_optional<RawType *>(m_returnType->get()) : std::nullopt;
        }


        void setModulePath(const std::vector<Token> &module_path);

        [[nodiscard]] std::vector<Token> modulePath() const {
            return m_namespacePrefix;
        }

        [[nodiscard]] const std::string &modulePathName() const;


        [[nodiscard]] const std::string &functionName() const;


        [[nodiscard]] FunctionArgument *getParam(const unsigned index) {
            if (index >= m_args.size()) {
                return nullptr;
            }
            return &m_args[index];
        }

        [[nodiscard]] const std::vector<FunctionArgument> &args() const;

        std::vector<FunctionArgument> &args();

        [[nodiscard]] std::shared_ptr<types::VariableType> asFunctionType() const;

        std::vector<std::unique_ptr<RawAnnotation> > &rawAnnotations() {
            return m_rawAnnotations;
        }

        [[nodiscard]] std::string functionSignature(bool withNamespace = true) const;

        [[nodiscard]] VisibilityModifier visibilityModifier() const {
            return m_visibilityModifier;
        }

        virtual std::unique_ptr<ast::FunctionDefinitionBase> cloneFunction() = 0;

        [[nodiscard]] virtual bool isMethod() const = 0;

        [[nodiscard]] virtual std::optional<types::VariableType *> parentStruct() const {
            return std::nullopt;
        }

        void setResolvedReturnType(std::shared_ptr<types::VariableType> type) {
            m_resolvedReturnTyoe = type;
        }

        [[nodiscard]] std::optional<std::shared_ptr<types::VariableType> > resolvedReturnType() const {
            return m_resolvedReturnTyoe;
        }
    };
}
