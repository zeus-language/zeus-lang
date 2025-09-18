#pragma once

#include <memory>
#include <vector>

#include "ASTNode.h"
#include "VariableDeclaration.h"
#include "lexer/Lexer.h"


namespace ast {
    struct FunctionArgument {
        std::string name;
        std::unique_ptr<RawType> rawType;

        std::optional<std::shared_ptr<types::VariableType> > type = std::nullopt;

        FunctionArgument(std::string name, std::unique_ptr<RawType> type)
            : name(std::move(name)), rawType(std::move(type)) {
        }
    };

    class FunctionDefinition final : public ASTNode {
        std::vector<FunctionArgument> m_args;
        std::optional<std::unique_ptr<RawType> > m_returnType;
        std::vector<std::unique_ptr<ASTNode> > m_statements;
        std::vector<Token> m_namespacePrefix;

    public:
        explicit FunctionDefinition(Token functionName, std::vector<FunctionArgument> args,
                                    std::optional<std::unique_ptr<RawType> > returnType,
                                    std::vector<std::unique_ptr<ASTNode> > statements);

        ~FunctionDefinition() override = default;

        [[nodiscard]] std::string functionName() const;

        std::vector<FunctionArgument> &args();

        std::vector<std::unique_ptr<ASTNode> > &statements() { return m_statements; }

        [[nodiscard]] std::optional<RawType *> returnType() const {
            return m_returnType.has_value() ? std::make_optional<RawType *>(m_returnType->get()) : std::nullopt;
        }

        void setModulePath(const std::vector<Token> &module_path) {
            m_namespacePrefix = module_path;
        }

        [[nodiscard]] std::vector<Token> modulePath() const {
            return m_namespacePrefix;
        }

        [[nodiscard]] std::string functionSignature() const;

        FunctionDefinition(FunctionDefinition &&) = default;

        FunctionDefinition(const FunctionDefinition &) = delete;

        FunctionDefinition &operator=(FunctionDefinition &&) = delete;

        FunctionDefinition &operator=(const FunctionDefinition &) = delete;
    };
} // ast

