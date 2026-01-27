#pragma once

#include <memory>
#include <vector>

#include "RawAnnotation.h"
#include "AnnotatedNode.h"
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
        std::vector<Token> m_namespacePrefix;
        std::vector<std::unique_ptr<RawAnnotation> > m_rawAnnotations;

    public:
        [[nodiscard]] std::optional<RawType *> returnType() const {
            return m_returnType.has_value() ? std::make_optional<RawType *>(m_returnType->get()) : std::nullopt;
        }

        void setModulePath(const std::vector<Token> &module_path) {
            m_namespacePrefix = module_path;
        }

        [[nodiscard]] std::vector<Token> modulePath() const {
            return m_namespacePrefix;
        }

        [[nodiscard]] std::string modulePathName() const {
            std::string name;
            for (auto &ns: m_namespacePrefix) {
                name += ns.lexical() + "::";
            }
            return name;
        }


        [[nodiscard]] std::string functionName() const;


        [[nodiscard]] FunctionArgument *getParam(const unsigned index) {
            if (index >= m_args.size()) {
                return nullptr;
            }
            return &m_args[index];
        }

        [[nodiscard]] const std::vector<FunctionArgument> &args()const ;
        std::vector<FunctionArgument> &args();

        [[nodiscard]] std::shared_ptr<types::VariableType> asFunctionType() const;

        std::vector<std::unique_ptr<RawAnnotation> > &rawAnnotations() {
            return m_rawAnnotations;
        }

        explicit FunctionDefinitionBase(Token functionName, std::vector<FunctionArgument> args,
                                        std::optional<std::unique_ptr<RawType> > returnType,
                                        std::vector<std::unique_ptr<RawAnnotation> > annotations) : AnnotatedNode(
                std::move(
                    functionName)),
            m_functionName(expressionToken().lexical()),
            m_args(std::move(args)),
            m_returnType(std::move(returnType)),
            m_rawAnnotations(std::move(annotations)) {
        }

        [[nodiscard]] std::string functionSignature(bool withNamespace = true) const;

        virtual std::unique_ptr<ast::FunctionDefinitionBase> cloneFunction() = 0;
    };

    class FunctionDefinition final : public FunctionDefinitionBase {
        std::vector<std::unique_ptr<ASTNode> > m_statements;
        std::optional<Token> genericParam;

    public:
        explicit FunctionDefinition(Token functionName, std::vector<FunctionArgument> args,
                                    std::optional<std::unique_ptr<RawType> > returnType,
                                    std::vector<std::unique_ptr<ASTNode> > statements,
                                    std::optional<Token> genericParam,
                                    std::vector<std::unique_ptr<RawAnnotation> > annotations);

        ~FunctionDefinition() override = default;


        std::vector<std::unique_ptr<ASTNode> > &statements() { return m_statements; }

        std::optional<ASTNode *> getVariableDefinition(const Token &token) const;


        FunctionDefinition(FunctionDefinition &&) = default;

        FunctionDefinition(const FunctionDefinition &) = delete;

        FunctionDefinition &operator=(FunctionDefinition &&) = delete;

        FunctionDefinition &operator=(const FunctionDefinition &) = delete;

        [[nodiscard]] std::optional<Token> getGenericParam() const {
            return genericParam;
        }
        std::unique_ptr<ast::FunctionDefinitionBase> cloneFunction() override;
    };
} // ast

